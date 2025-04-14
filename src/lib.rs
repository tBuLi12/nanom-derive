use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{ext::IdentExt, Data, DataEnum, DataStruct, DeriveInput, Fields, GenericParam};

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
        let name = field
            .ident
            .as_ref()
            .unwrap()
            .unraw()
            .to_string()
            .to_case(Case::Camel);

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
        Data::Struct(DataStruct { fields, .. }) => fields_to_object(fields, quote! { self. }),
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

                let object = fields_to_object(&variant.fields, quote! {});

                quote! { Self::#name_ident { #(#bindings),* } => ::nanom::create_enum(env, #name, #object) }
            });

            quote! {
                unsafe {
                    match self {
                        #(#match_variants,)*
                    }?
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

fn fields_to_object(
    fields: &Fields,
    field_name_prefix: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let fields = match fields {
        Fields::Named(fields) => fields,
        Fields::Unnamed(_) => panic!("Tuple structs cannot be used to derive JsObject"),
        Fields::Unit => panic!("Unit structs cannot be used to derive JsObject"),
    };

    let set_fields = fields.named.iter().map(|field| {
        let name_ident = field.ident.as_ref().unwrap();
        let name = name_ident.unraw().to_string().to_case(Case::Camel);
        quote! { .field(#name, #field_name_prefix #name_ident)? }
    });

    quote! {
        unsafe { ::nanom::object_builder(env)? #(#set_fields)*.build() }
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
                let object = ::nanom::object_accessor(env, value);
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
                    let enum_value = ::nanom::enum_accessor(env, value);
                    let object = enum_value.fields()?;
                    match &*enum_value.variant()? {
                        #(#match_variants,)*
                        _ => return ::std::result::Result::Err(::nanom::ConversionError::InvalidKind(enum_value.variant()?)),
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
        let name = name_ident.unraw().to_string().to_case(Case::Camel);
        quote! { #name_ident: object.get(#name)? }
    });

    quote! { { #(#define_fields,)* } }
}

#[proc_macro_derive(AsJs)]
pub fn derive_as_js(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as DeriveInput);

    let name_ident = input.ident;
    let generic_params = input.generics.params;
    let where_clause = input.generics.where_clause;

    let body = match &input.data {
        Data::Union(_) => panic!("Unions cannot be used to derive JsObject"),
        Data::Struct(DataStruct { fields, .. }) => fields_to_object(fields, quote! { &self. }),
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

                let object = fields_to_object(&variant.fields, quote! {});

                quote! { Self::#name_ident { #(#bindings),* } => ::nanom::create_enum(env, #name, #object) }
            });

            quote! {
                unsafe {
                    match self {
                        #(#match_variants,)*
                    }?
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
        impl<#generic_params> ::nanom::IntoJs for &#name_ident<#(#generic_args),*> #where_clause {
            fn into_js(self, env: ::nanom::napi::Env) -> ::std::result::Result<::nanom::napi::Value, ::nanom::ConversionError> {
                ::std::result::Result::Ok(unsafe { #body })
            }
        }
    }
    .into()
}
