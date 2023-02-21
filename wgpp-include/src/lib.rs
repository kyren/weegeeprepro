use std::{
    collections::HashSet,
    env,
    fs::File,
    io::{self, Read},
    path::{Path, PathBuf},
};

use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, LitStr, Result as ParseResult, Token,
};

struct IncludeWgsl {
    sources: Vec<String>,
    content: String,
}

impl Parse for IncludeWgsl {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let mut includes: Vec<String> = Vec::new();
        let mut shader_root = "".to_owned();
        let mut definitions: Vec<(String, Option<String>)> = Vec::new();

        while input.peek(Ident) {
            let key = input.parse::<Ident>()?;
            match key.to_string().as_str() {
                "include" => {
                    input.parse::<Token![:]>()?;

                    includes.push(input.parse::<LitStr>()?.value());
                }
                "root" => {
                    input.parse::<Token![:]>()?;

                    shader_root = input.parse::<LitStr>()?.value();
                }
                "define" => {
                    input.parse::<Token![:]>()?;

                    let name = input.parse::<Ident>()?;
                    let value = if input.peek(Token![=]) {
                        input.parse::<Token![=]>()?;
                        Some(input.parse::<LitStr>()?.value())
                    } else {
                        None
                    };
                    definitions.push((name.to_string(), value));
                }
                _ => {
                    return Err(input.error("unknown option"));
                }
            }

            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            } else {
                break;
            }
        }

        let base_path = Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap()).join(shader_root);

        let mut shader_files = FsFileProvider::new(base_path);

        let mut preprocessor = wgpp::Preprocessor::new(&mut shader_files);
        for (name, value) in definitions {
            preprocessor.define(&name, value.as_deref().unwrap_or(""));
        }
        for include in &includes {
            preprocessor
                .include_file(include)
                .map_err(|e| input.error(e))?;
        }
        let content = preprocessor.finish();

        let mut sources = Vec::new();
        for included_path in &shader_files.included_paths {
            sources.push(
                shader_files
                    .base_path
                    .join(included_path)
                    .to_str()
                    .ok_or_else(|| input.error("non-utf8 path"))?
                    .to_owned(),
            );
        }

        let path = format!("<{}>", includes.join(" + "));

        let module = naga::front::wgsl::parse_str(&content)
            .map_err(|e| input.error(e.emit_to_string_with_path(&content, &path)))?;

        naga::valid::Validator::new(
            naga::valid::ValidationFlags::all(),
            naga::valid::Capabilities::all(),
        )
        .validate(&module)
        .map_err(|e| input.error(e.emit_to_string_with_path(&content, &path)))?;

        Ok(IncludeWgsl { sources, content })
    }
}

#[proc_macro]
pub fn include_wgsl(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let IncludeWgsl { sources, content } = parse_macro_input!(tokens as IncludeWgsl);
    quote! {
        {
            #({ const _FORCE_DEP: &[u8] = include_bytes!(#sources); })*
            #content
        }
    }
    .into()
}

struct FsFileProvider {
    base_path: PathBuf,
    included_paths: HashSet<String>,
}

impl FsFileProvider {
    fn new(base_path: PathBuf) -> Self {
        FsFileProvider {
            base_path,
            included_paths: HashSet::new(),
        }
    }
}

impl wgpp::FileProvider for FsFileProvider {
    type Error = io::Error;

    fn load_file(&mut self, path: &str) -> Result<String, Self::Error> {
        self.included_paths.insert(path.to_owned());
        let mut file = File::open(Path::new(&self.base_path).join(path))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        Ok(contents)
    }
}
