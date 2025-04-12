use std::iter;

use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use proc_macro2::{Group, Punct, TokenTree};
use quote::{quote, ToTokens};
use syn::{ext::IdentExt, Data, DataEnum, DataStruct, DeriveInput, Fields, GenericParam, Token};

#[proc_macro_derive(TsType)]
pub fn derive_ts_type(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as DeriveInput);

    let name_ident = input.ident;
    let type_name = name_ident.unraw().to_string();
    let generic_params = input.generics.params;
    let where_clause = input.generics.where_clause;

    let body = match &input.data {
        Data::Union(_) => panic!("Unions cannot be used to derive JsObject"),
        Data::Struct(DataStruct { fields, .. }) => {
            let fields_map = fields_to_types_map(&fields);

            quote! {
                ::nanom::typing::Type::NamedObject {
                    name: #type_name.to_string(),
                    fields: #fields_map,
                    id: <Self as ::nanom::TsType>::ts_type_ref(),
                }
            }
        }
        Data::Enum(DataEnum { variants, .. }) => {
            let insert_kinds = variants.into_iter().map(|variant| {
                let name_str = variant.ident.unraw().to_string();
                let fields_map = fields_to_types_map(&variant.fields);

                quote! { enum_kinds.insert(#name_str.to_string(), #fields_map); }
            });

            quote! {
                ::nanom::typing::Type::Enum {
                    name: #type_name.to_string(),
                    kinds: {
                        let mut enum_kinds = ::std::collections::HashMap::new();
                        #(#insert_kinds)*
                        enum_kinds
                    },
                    id: <Self as ::nanom::TsType>::ts_type_ref()
                }
            }
        }
    };

    let generic_args = generic_params.iter().map(|param| match param {
        GenericParam::Type(type_param) => type_param.ident.to_token_stream(),
        GenericParam::Lifetime(lifetime) => lifetime.lifetime.to_token_stream(),
        GenericParam::Const(const_param) => {
            let ident = &const_param.ident;
            let colon = const_param.colon_token;
            let ty = &const_param.ty;
            quote! { #ident #colon #ty }
        }
    });

    quote! {
        impl<#generic_params> ::nanom::TsType for #name_ident<#(#generic_args),*> #where_clause {
            fn ts_type() -> ::nanom::typing::Type {
                #body
            }
        }
    }
    .into()
}

fn fields_to_types_map(fields: &Fields) -> proc_macro2::TokenStream {
    let fields = match fields {
        Fields::Named(fields) => fields,
        Fields::Unnamed(_) => panic!("Tuple structs cannot be used to derive JsObject"),
        Fields::Unit => panic!("Unit structs cannot be used to derive JsObject"),
    };

    let insert_fields = fields.named.iter().map(|field| {
        let name = field.ident.as_ref().unwrap().unraw().to_string();
        let ty = &field.ty;
        quote! { fields.insert(#name.to_string(), <#ty as ::nanom::TsType>::ts_type_ref()); }
    });

    quote! {
        {
            let mut fields = ::std::collections::HashMap::new();
            #(#insert_fields)*
            fields
        }
    }
}

#[proc_macro_derive(IntoJs)]
pub fn derive_into_js(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as DeriveInput);

    let name_ident = input.ident;
    let generic_params = input.generics.params;
    let where_clause = input.generics.where_clause;

    let body = match &input.data {
        Data::Union(_) => panic!("Unions cannot be used to derive JsObject"),
        Data::Struct(DataStruct { fields, .. }) => fields_to_object(fields),
        Data::Enum(DataEnum { variants, .. }) => {
            let match_variants = variants.into_iter().map(|variant| {
                let name_ident = &variant.ident;
                let name = name_ident.unraw().to_string();
                let fields = match &variant.fields {
                    Fields::Named(fields) => fields,
                    Fields::Unnamed(_) => panic!("Tuple structs cannot be used to derive JsObject"),
                    Fields::Unit => panic!("Unit structs cannot be used to derive JsObject"),
                };

                let bindings = fields
                    .named
                    .iter()
                    .map(|field| field.ident.as_ref().unwrap());

                let object = fields_to_object(&variant.fields);

                quote! { Self::#name_ident { #(#bindings),* } => ::nanom::create_enum(env, #name, #object) }
            });

            quote! {
                unsafe {
                    match self {
                        #(#match_variants,)*
                    }
                }
            }
        }
    };

    let generic_args = generic_params.iter().map(|param| match param {
        GenericParam::Type(type_param) => type_param.ident.to_token_stream(),
        GenericParam::Lifetime(lifetime) => lifetime.lifetime.to_token_stream(),
        GenericParam::Const(const_param) => {
            let ident = &const_param.ident;
            let colon = const_param.colon_token;
            let ty = &const_param.ty;
            quote! { #ident #colon #ty }
        }
    });

    quote! {
        impl<#generic_params> ::nanom::IntoJs for #name_ident<#(#generic_args),*> #where_clause {
            fn into_js(self, env: ::nanom::napi::Env) -> ::std::result::Result<::nanom::napi::Value, ::nanom::ConversionError> {
                ::std::result::Result::Ok(unsafe { #body })
            }
        }
    }
    .into()
}

fn fields_to_object(fields: &Fields) -> proc_macro2::TokenStream {
    let fields = match fields {
        Fields::Named(fields) => fields,
        Fields::Unnamed(_) => panic!("Tuple structs cannot be used to derive JsObject"),
        Fields::Unit => panic!("Unit structs cannot be used to derive JsObject"),
    };

    let set_fields = fields.named.iter().map(|field| {
        let name_ident = field.ident.as_ref().unwrap();
        let name = name_ident.unraw().to_string();
        let ty = &field.ty;
        quote! { .field(#name, <#ty as ::nanom::IntoJs>::into_js(self.#name_ident, env)?) }
    });

    quote! {
        unsafe { ::nanom::object_builder() #(#set_fields)* }
    }
}

#[proc_macro_derive(FromJs)]
pub fn derive_from_js(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as DeriveInput);

    let name_ident = input.ident;
    let generic_params = input.generics.params;
    let where_clause = input.generics.where_clause;

    let body = match &input.data {
        Data::Union(_) => panic!("Unions cannot be used to derive JsObject"),
        Data::Struct(DataStruct { fields, .. }) => {
            let fields = fields_to_struct(fields);
            quote! {
                Self #fields
            }
        }
        Data::Enum(DataEnum { variants, .. }) => {
            let match_variants = variants.into_iter().map(|variant| {
                let name_ident = &variant.ident;
                let name = name_ident.unraw().to_string();
                let fields = fields_to_struct(&variant.fields);

                quote! { #name => Self::#name_ident #fields }
            });

            quote! {
                unsafe {
                    match self {
                        #(#match_variants,)*
                    }
                }
            }
        }
    };

    let generic_args = generic_params.iter().map(|param| match param {
        GenericParam::Type(type_param) => type_param.ident.to_token_stream(),
        GenericParam::Lifetime(lifetime) => lifetime.lifetime.to_token_stream(),
        GenericParam::Const(const_param) => {
            let ident = &const_param.ident;
            let colon = const_param.colon_token;
            let ty = &const_param.ty;
            quote! { #ident #colon #ty }
        }
    });

    quote! {
        impl<#generic_params> ::nanom::FromJs for #name_ident<#(#generic_args),*> #where_clause {
            fn from_js(env: ::nanom::napi::Env, value: ::nanom::napi::Value) -> ::std::result::Result<Self, ::nanom::ConversionError> {
                let enum_value = ::nanom::enum_accessor(env, value)?;
                let object = enum_value.fields()?;
                ::std::result::Result::Ok(unsafe { #body })
            }
        }
    }
    .into()
}

fn fields_to_struct(fields: &Fields) -> proc_macro2::TokenStream {
    let fields = match fields {
        Fields::Named(fields) => fields,
        Fields::Unnamed(_) => panic!("Tuple structs cannot be used to derive JsObject"),
        Fields::Unit => panic!("Unit structs cannot be used to derive JsObject"),
    };

    let define_fields = fields.named.iter().map(|field| {
        let name_ident = field.ident.as_ref().unwrap();
        let name = name_ident.unraw().to_string();
        quote! { #name: object.get(#name)? }
    });

    quote! { { #(#define_fields,)* } }
}

// fn ts_type_body(input: &Structure) -> proc_macro2::TokenStream {
//     let type_name = input.ast().ident.unraw().to_string();

//     match &input.ast().data {
//         Data::Union(_) => panic!("Unions cannot be used to derive JsObject"),
//         Data::Struct(DataStruct { fields, .. }) => {
//             let fields = fields_ts_type(&fields);
//             quote! { ::nanom::typing::Type::NamedObject { name: #type_name.to_string(), fields: #fields, id: ::nanom::typing::TypeRef { get_type: <Self as ::nanom::TsType>::ts_type } } }
//         }
//         Data::Enum(DataEnum { variants, .. }) => {
//             let add_variants = variants.into_iter().map(|variant| {
//                 let name_str = variant.ident.unraw().to_string();

//                 let fields_type = fields_ts_type(&variant.fields);

//                 quote! {
//                     {
//                         enum_fields.insert(#name_str.to_string(), #fields_type);
//                     }
//                 }
//             });

//             quote! {
//                 {
//                     let mut enum_fields = ::std::collections::HashMap::new();
//                     #(#add_variants)*
//                     ::nanom::typing::Type::Enum { kinds: enum_fields, name: #type_name.to_string(), id: ::nanom::typing::TypeRef { get_type: <Self as ::nanom::TsType>::ts_type } }
//                 }
//             }
//         }
//     }
// }

// fn into_js_body(input: &mut Structure) -> proc_macro2::TokenStream {
//     input.bind_with(|_| BindStyle::Ref);

//     let kind = input.each_variant(|variant| {
//         let name = variant.ast().ident.unraw().to_string();
//         quote! { env.create_string(#name)? }
//     });

//     input.bind_with(|_| BindStyle::Move);

//     let set_props = input.each(|field| {
//         let name = field.ast().ident.as_ref().expect("Tuple structs cannot be used to derive JsObject").unraw().to_string();
//         let name_camel = name.to_case(Case::Camel);
//         let binding = &field.binding;

//         quote! {
//             (|| -> ::std::result::Result<(), ::nanom::ConversionError> {
//             env.set_property(object, env.create_string(#name_camel)?, ::nanom::IntoJs::into_js(#binding, env)?)?;
//             ::std::result::Result::Ok(())
//             })().map_err(|err| ::nanom::ConversionError::InObjectField { error: Box::new(err), field_name: #name })?;
//         }
//     });

//     let get_object = quote! {
//         {
//             let mut object = env.create_object()?;

//             match self {
//                 #set_props
//             }

//             object
//         }
//     };

//      match input.ast().data {
//         Data::Union(_) => panic!("Unions cannot be used to derive JsObject"),
//         Data::Struct(_) => get_object,
//         Data::Enum(_) => quote! {
//             {
//                 let mut object = env.create_object()?;

//                 (|| -> ::std::result::Result<(), ::nanom::napi::Status> {
//                     env.set_property(object, env.create_string("kind")?, match self { #kind })?;
//                     ::std::result::Result::Ok(())
//                 })().map_err(::nanom::ConversionError::InKind)?;

//                 (|| -> ::std::result::Result<(), ::nanom::ConversionError> {
//                     env.set_property(object, env.create_string("fields")?, #get_object)?;
//                     ::std::result::Result::Ok(())
//                 })().map_err(|err|::nanom::ConversionError::InEnumValue(Box::new(err)))?;

//                 object
//             }
//         },
//     }
// }

// fn from_js_body(input: &Structure) -> proc_macro2::TokenStream {
//     match &input.ast().data {
//         Data::Union(_) => panic!("Unions cannot be used to derive JsObject"),
//         Data::Struct(DataStruct { fields, .. }) => {
//             let parse_fields = parse_fields(&fields);

//             quote! {
//                 {
//                     Self { #parse_fields }
//                 }
//             }
//         }
//         Data::Enum(DataEnum { variants, .. }) => {
//             let parse_variants = variants.into_iter().map(|variant| {
//                 let name = &variant.ident;
//                 let name_str = name.unraw().to_string();

//                 let parse_fields = parse_fields(&variant.fields);

//                 quote! {
//                     #name_str => (|| -> ::std::result::Result<_, ::nanom::ConversionError> {
//                         ::std::result::Result::Ok(Self::#name { #parse_fields })
//                     })().map_err(|err|::nanom::ConversionError::InEnumValue(Box::new(err)))?,
//                 }
//             });

//             quote! {
//                 {
//                     let kind = (|| -> ::std::result::Result<_, ::nanom::napi::Status> {
//                         ::std::result::Result::Ok(env.get_value_string(env.get_property(value, env.create_string("kind")?)?)?)
//                     })().map_err(::nanom::ConversionError::InKind)?;

//                     let value = (|| -> ::std::result::Result<_, ::nanom::ConversionError> {
//                         ::std::result::Result::Ok(env.get_property(value, env.create_string("fields")?)?)
//                     })().map_err(|err|::nanom::ConversionError::InEnumValue(Box::new(err)))?;

//                     match kind.as_str() {
//                         #(#parse_variants)*
//                         _ => return ::std::result::Result::Err(::nanom::ConversionError::InvalidKind(kind.to_string())),
//                     }
//                 }
//             }
//         }
//     }
// }

// fn derive_ts_type(mut input: Structure) -> TokenStream {
//     for ty in input.referenced_ty_params() {
//         input.add_where_predicate(syn::parse_quote! { #ty: ::nanom::TsType });
//     }

//     let ts_type = ts_type_body(&input);

//     input
//         .unbound_impl(quote!(::nanom::TsType), quote! {
//             fn ts_type() -> ::nanom::typing::Type {
//                 #ts_type
//             }
//         })
//         .into()
// }

// fn derive_into_js(mut input: Structure) -> TokenStream {
//     for ty in input.referenced_ty_params() {
//         input.add_where_predicate(syn::parse_quote! { #ty: ::nanom::IntoJs });
//     }

//     let to_js = into_js_body(&mut input);

//     input
//         .unbound_impl(quote!(::nanom::IntoJs), quote! {
//             fn into_js(self, env: ::nanom::napi::Env) -> ::std::result::Result<::nanom::napi::Value, ::nanom::ConversionError> {
//                 ::std::result::Result::Ok(unsafe #to_js)
//             }
//         })
//         .into()
// }

// fn derive_from_js(mut input: Structure) -> TokenStream {
//     for ty in input.referenced_ty_params() {
//         input.add_where_predicate(syn::parse_quote! { #ty: ::nanom::FromJs });
//     }

//     let from_js = from_js_body(&input);

//     input
//         .unbound_impl(quote!(::nanom::FromJs), quote! {
//             fn from_js(env: ::nanom::napi::Env, value: ::nanom::napi::Value) -> ::std::result::Result<Self, ::nanom::ConversionError> {
//                 ::std::result::Result::Ok(unsafe #from_js)
//             }
//         })
//         .into()
// }

// fn derive_js_object(mut input: Structure) -> TokenStream {
//     for ty in input.referenced_ty_params() {
//         input.add_where_predicate(syn::parse_quote! { #ty: ::nanom::FromJs + ::nanom::IntoJs });
//         input.add_where_predicate(syn::parse_quote! { for<'a> &'a #ty: ::nanom::IntoJs });
//     }

//     let to_js = into_js_body(&mut input);
//     let from_js = from_js_body(&input);
//     let ts_type = ts_type_body(&input);

//     let tokens = input
//         .unbound_impl(quote!(::nanom::JsObject), quote! {
//             fn into_js(&self, env: ::nanom::napi::Env) -> ::std::result::Result<::nanom::napi::Value, ::nanom::ConversionError> {
//                 ::std::result::Result::Ok(unsafe #to_js)
//             }

//             fn from_js(env: ::nanom::napi::Env, value: ::nanom::napi::Value) -> ::std::result::Result<Self, ::nanom::ConversionError> {
//                 ::std::result::Result::Ok(unsafe #from_js)
//             }

//             fn ts_type() -> ::nanom::typing::Type {
//                 #ts_type
//             }
//         });

//     let tokens2 = input
//         .unbound_impl(quote!(::nanom::IntoJs), quote! {
//             fn into_js(self, env: ::nanom::napi::Env) -> ::std::result::Result<::nanom::napi::Value, ::nanom::ConversionError> {
//                 ::std::result::Result::Ok(unsafe #to_js)
//             }
//         });

//         if tokens2.is_empty() {panic!("")}

//     let tokens3 = input
//         .unbound_impl(quote!(::nanom::TsType), quote! {
//             fn ts_type() -> ::nanom::typing::Type {
//                 #ts_type
//             }
//         });

//     tokens.into_stream().into_iter().chain(add_at_before_for(tokens2).into_stream().into_iter()).chain(add_at_before_for(tokens3).into_stream().into_iter()).collect()
// }

// fn add_at_before_for(mut tokens: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
//     fn add_at_before_for_inner(mut tokens: proc_macro2::TokenStream, first: &mut bool) -> proc_macro2::TokenStream {
//         let mut tokens = tokens.into_iter();
//         let mut result: Vec<TokenTree> = vec![];

//         for token in tokens {
//             match &token {
//                 TokenTree::Ident(ident) if ident.to_string() == "for" && *first => {
//                     println!("inserting for");
//                     result.push(token);
//                     result.push(TokenTree::Punct(Punct::new('&', proc_macro2::Spacing::Alone)));
//                     *first = false;
//                 }
//                 TokenTree::Group(group) if *first => {
//                     result.push(TokenTree::Group(Group::new(group.delimiter(), add_at_before_for_inner(group.stream(), first))));
//                 }
//                 _ => {
//                     println!("not inserting for: {}", token);
//                     result.push(token);
//                 }
//             }
//         }
//         result.into_iter().collect()
//     }

//     add_at_before_for_inner(tokens, &mut true)
// }

// fn parse_fields(fields: &Fields) -> proc_macro2::TokenStream {
//     let Fields::Named(fields) = fields else {
//         panic!("Only named structs can be used to derive JsObject");
//     };

//     let fields = fields.named.iter().map(|field| {
//         let name = field.ident.as_ref().unwrap();
//         let name_str = name.unraw().to_string();
//         let name_str_camel = name_str.to_case(Case::Camel);

//         quote! {
//             #name: (|| -> ::std::result::Result<_, ::nanom::ConversionError> {
//                 ::nanom::FromJs::from_js(env, env.get_property(value, env.create_string(#name_str_camel)?)?)
//             })().map_err(|err| ::nanom::ConversionError::InObjectField { error: Box::new(err), field_name: #name_str })?,
//         }
//     });

//     quote! {
//         #(#fields)*
//     }
// }

// fn  fields_ts_type(fields: &Fields) -> proc_macro2::TokenStream {
//     let Fields::Named(fields) = fields else {
//         panic!("Only named structs can be used to derive JsObject");
//     };

//     let fields = fields.named.iter().map(|field| {
//         let name = field.ident.as_ref().unwrap().unraw().to_string().to_case(Case::Camel);
//         let ty = &field.ty;

//         quote! {
//             fields.insert(#name.to_string(), <#ty as ::nanom::TsType>::ts_type_ref());
//         }
//     });

//     quote! {
//         {
//             let mut fields = ::std::collections::HashMap::new();
//             #(#fields)*
//             fields
//         }
//     }
// }

// synstructure::decl_derive!([TsType] => derive_ts_type);
// synstructure::decl_derive!([FromJs] => derive_from_js);
// synstructure::decl_derive!([IntoJs] => derive_into_js);
// synstructure::decl_derive!([JsObject] => derive_js_object);
