use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use syn::{Data, DataEnum, DataStruct, Fields};
use synstructure::{quote, BindStyle, Structure};

fn derive_js_object(mut input: Structure) -> TokenStream {
    for ty in input.referenced_ty_params() {
        input.add_where_predicate(syn::parse_quote! { #ty: ::nanom::IntoJs + ::nanom::FromJs });
    }

    let kind = input.each_variant(|variant| {
        let name = variant.ast().ident.to_string();
        quote! { env.create_string(#name)? }
    });

    input.bind_with(|_| BindStyle::Move);

    let set_props = input.each(|field| {
        let name = field.ast().ident.as_ref().expect("Tuple structs cannot be used to derive JsObject").to_string();
        let name_camel = name.to_case(Case::Camel);
        let binding = &field.binding;

        quote! {
            (|| -> ::std::result::Result<(), ::nanom::ConversionError> {
            env.set_property(object, env.create_string(#name_camel)?, ::nanom::IntoJs::into_js(#binding, env)?)?;
            ::std::result::Result::Ok(())
            })().map_err(|err| ::nanom::ConversionError::InObjectField { error: Box::new(err), field_name: #name })?;
        }
    });

    let get_object = quote! {
        {
            let mut object = env.create_object()?;

            match self {
                #set_props
            }

            object
        }
    };

    let to_js = match input.ast().data {
        Data::Union(_) => panic!("Unions cannot be used to derive JsObject"),
        Data::Struct(_) => get_object,
        Data::Enum(_) => quote! {
            {
                let mut object = env.create_object()?;

                (|| -> ::std::result::Result<(), ::nanom::napi::Status> {
                    env.set_property(object, env.create_string("kind")?, match self { #kind })?;
                    ::std::result::Result::Ok(())
                })().map_err(::nanom::ConversionError::InKind)?;

                (|| -> ::std::result::Result<(), ::nanom::ConversionError> {
                    env.set_property(object, env.create_string("fields")?, #get_object)?;
                    ::std::result::Result::Ok(())
                })().map_err(|err|::nanom::ConversionError::InEnumValue(Box::new(err)))?;

                object
            }
        },
    };

    let from_js = match &input.ast().data {
        Data::Union(_) => panic!("Unions cannot be used to derive JsObject"),
        Data::Struct(DataStruct { fields, .. }) => {
            let parse_fields = parse_fields(&fields);

            quote! {
                {
                    Self { #parse_fields }
                }
            }
        }
        Data::Enum(DataEnum { variants, .. }) => {
            let parse_variants = variants.into_iter().map(|variant| {
                let name = &variant.ident;
                let name_str = name.to_string();

                let parse_fields = parse_fields(&variant.fields);

                quote! {
                    #name_str => (|| -> ::std::result::Result<_, ::nanom::ConversionError> {
                        ::std::result::Result::Ok(Self::#name { #parse_fields })
                    })().map_err(|err|::nanom::ConversionError::InEnumValue(Box::new(err)))?,
                }
            });

            quote! {
                {
                    let kind = (|| -> ::std::result::Result<_, ::nanom::napi::Status> {
                        ::std::result::Result::Ok(env.get_value_string(env.get_property(value, env.create_string("kind")?)?)?)
                    })().map_err(::nanom::ConversionError::InKind)?;

                    let value = (|| -> ::std::result::Result<_, ::nanom::ConversionError> {
                        ::std::result::Result::Ok(env.get_property(value, env.create_string("fields")?)?)
                    })().map_err(|err|::nanom::ConversionError::InEnumValue(Box::new(err)))?;

                    match kind.as_str() {
                        #(#parse_variants)*
                        _ => return ::std::result::Result::Err(::nanom::ConversionError::InvalidKind(kind.to_string())),
                    }
                }
            }
        }
    };

    let type_name = input.ast().ident.to_string();

    let ts_type = match &input.ast().data {
        Data::Union(_) => panic!("Unions cannot be used to derive JsObject"),
        Data::Struct(DataStruct { fields, .. }) => {
            let fields = fields_ts_type(&fields);
            quote! { ::nanom::typing::Type::NamedObject { name: #type_name.to_string(), fields: #fields, id: ::std::any::TypeId::of::<Self>() } }
        }
        Data::Enum(DataEnum { variants, .. }) => {
            let add_variants = variants.into_iter().map(|variant| {
                let name_str = variant.ident.to_string();

                let fields_type = fields_ts_type(&variant.fields);

                quote! {
                    {
                        enum_fields.insert(#name_str.to_string(), #fields_type); 
                    }
                }
            });

            quote! {
                {
                    let mut enum_fields = ::std::collections::HashMap::new();
                    #(#add_variants)*
                    ::nanom::typing::Type::Enum { kinds: enum_fields, name: #type_name.to_string(), id: ::std::any::TypeId::of::<Self>() }
                }
            }
        }
    };

    input
        .unbound_impl(quote!(::nanom::JsObject), quote! {
            fn into_js(self, env: ::nanom::napi::Env) -> ::std::result::Result<::nanom::napi::Value, ::nanom::ConversionError> {
                ::std::result::Result::Ok(unsafe #to_js)
            }
            
            fn from_js(env: ::nanom::napi::Env, value: ::nanom::napi::Value) -> ::std::result::Result<Self, ::nanom::ConversionError> {
                ::std::result::Result::Ok(unsafe #from_js)
            }

            fn ts_type() -> ::nanom::typing::Type {
                #ts_type
            }
        })
        .into()
}

fn parse_fields(fields: &Fields) -> proc_macro2::TokenStream {
    let Fields::Named(fields) = fields else {
        panic!("Only named structs can be used to derive JsObject");
    };

    let fields = fields.named.iter().map(|field| {
        let name = field.ident.as_ref().unwrap();
        let name_str = name.to_string();
        let name_str_camel = name_str.to_case(Case::Camel);

        quote! {
            #name: (|| -> ::std::result::Result<_, ::nanom::ConversionError> {
                ::nanom::FromJs::from_js(env, env.get_property(value, env.create_string(#name_str_camel)?)?)
            })().map_err(|err| ::nanom::ConversionError::InObjectField { error: Box::new(err), field_name: #name_str })?,
        }
    });

    quote! {
        #(#fields)*
    }
}

fn fields_ts_type(fields: &Fields) -> proc_macro2::TokenStream {
    let Fields::Named(fields) = fields else {
        panic!("Only named structs can be used to derive JsObject");
    };

    let fields = fields.named.iter().map(|field| {
        let name = field.ident.as_ref().unwrap().to_string().to_case(Case::Camel);
        let ty = &field.ty;

        quote! {
            fields.insert(#name.to_string(), <#ty as ::nanom::TsType>::ts_type());
        }
    });

    quote! {
        {
            let mut fields = ::std::collections::HashMap::new();
            #(#fields)*
            fields
        }
    }
}

synstructure::decl_derive!([JsObject] => derive_js_object);
