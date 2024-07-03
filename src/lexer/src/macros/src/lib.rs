use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, punctuated::Punctuated, token::Comma, Data, DeriveInput, Fields, FieldsNamed, GenericParam, Generics, ItemStruct};

fn get_stripped_generics(generics: &Generics) -> proc_macro2::TokenStream {
    let generics = generics.params.iter().map(|param| {
        let mut param = param.clone();
        match param {
            GenericParam::Lifetime(ref mut l) => l.bounds = Punctuated::new(),
            GenericParam::Type(ref mut t) => t.bounds = Punctuated::new(),
            _ => {}
        };
        quote!(#param)
    });
    quote!(< #( #generics ,)* >)
}

#[proc_macro_attribute]
pub fn spanned(_args: TokenStream, input: TokenStream) -> TokenStream  {
    let mut ast = parse_macro_input!(input as ItemStruct);
    let vis = &ast.vis;
    let mut created = false;
    let named = match ast.fields {
        Fields::Named(FieldsNamed{ref mut named, ..}) => named,
        _ => {
            created = true;
            let brace = quote!({}).into();
            let fields = parse_macro_input!(brace as FieldsNamed);
            ast.fields = Fields::Named(fields);
            let Fields::Named(FieldsNamed{ref mut named, ..}) = ast.fields else { panic!() };
            named
        }
    };
    let f = quote!({
        #[constructor = false]
        #vis span: std::option::Option<lexer::Span> }).into();
    let f = parse_macro_input!(f as FieldsNamed);
    if !named.trailing_punct() && !created {
        let comma = Comma::default();
        named.push_punct(comma);
    }
    named.push_value(f.named.last().cloned().unwrap());

    quote! {
        #[derive(lexer::Spanned,builders::Constructor)]
        #ast
    }.into()
}

#[proc_macro_derive(Spanned)]
pub fn spanned_derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let generics = &ast.generics;
    let name = &ast.ident;
    let wher = &generics.where_clause;
    let stripped_generics = get_stripped_generics(generics);

    match &ast.data {
        Data::Struct(_) => quote!{
            impl #generics lexer::Spannable for #name #stripped_generics #wher {
                fn get_span(&self) -> std::option::Option<lexer::Span> {
                    self.span
                }
                fn set_span(&mut self, span: lexer::Span) {
                    self.span = Some(span);
                }
            }
        },
        Data::Enum(e) => {
            let get_variants = e.variants.iter().map(|v| {
                let ident = &v.ident;
                quote!( Self :: #ident (i) => i.get_span().clone()  )
            });
            let set_variants = e.variants.iter().map(|v| {
                let ident = &v.ident;
                quote!( Self :: #ident (i) => i.set_span(span)  )
            });
            quote! {
                impl #generics lexer::Spannable for #name #stripped_generics #wher {
                    fn get_span(&self) -> std::option::Option<lexer::Span> {
                        match self {
                            #( #get_variants ,)*
                        }
                    }
                    fn set_span(&mut self, span: lexer::Span) {
                        match self {
                            #( #set_variants ,)*
                        }
                    }
                }
            }
        },
        _ => panic!()
    }.into()
}
