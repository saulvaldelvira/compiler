#[macro_use]
extern crate json;
use core::ptr::read;

use hir::hir_id::HirNode;
use json::Json;
use semantic::Semantic;

trait JsonSerialize {
    fn serialize(&self, hir: &hir::Session<'_>, sem: &Semantic<'_>) -> Json;
}

pub fn serialize_json(hir: &hir::Session<'_>, sem: &Semantic<'_>) -> String {
    let json = hir.get_root().serialize(hir, sem);
    json.to_string()
}

impl JsonSerialize for hir::Module<'_> {
    fn serialize(&self, hir: &hir::Session<'_>, sem: &Semantic<'_>) -> Json {
        let mut map = json::Map::new();

        self.name.ident.sym.borrow(|name| {
            map.insert(Box::from("name"), Json::String(name.into()));
        });

        let items = self.items.iter().map(|item| {
            item.serialize(hir, sem)
        }).collect();
        map.insert(Box::from("items"), Json::Array(items));

        Json::Object(map)
    }
}

impl JsonSerialize for hir::Function<'_> {
    fn serialize(&self, hir: &hir::Session<'_>, sem: &Semantic<'_>) -> Json {
        json!({
            "kind": "Function",
            "is_extern": {self.is_extern},
            "is_variadic": {self.is_variadic},
            "name": {self.name.ident.sym.to_string()},
            "params": {
                Json::Array(self.params.iter().map(|p| p.serialize(hir, sem)).collect())
            },
            "ret_type": {self.ret_ty.serialize(hir, sem)},
            "body": {self.body.map(|b| b.serialize(hir, sem)).unwrap_or(Json::Null)}

        })
    }
}

impl JsonSerialize for hir::Param<'_> {
    fn serialize(&self, hir: &hir::Session<'_>, sem: &Semantic<'_>) -> Json {
        json!({
            "kind": "Param",
            "name": {self.name.ident.sym.to_string()},
            "type": {self.ty.serialize(hir, sem)},
        })
    }
}

impl JsonSerialize for hir::UseItem<'_> {
    fn serialize(&self, hir: &hir::Session<'_>, sem: &Semantic<'_>) -> Json {
        Json::Null
    }
}

impl JsonSerialize for hir::Item<'_> {
    fn serialize(&self, hir: &hir::Session<'_>, sem: &Semantic<'_>) -> Json {
        let (mut obj, kind) = match self.kind {
            hir::ItemKind::Mod(module) => (module.serialize(hir, sem), "Module"),
            hir::ItemKind::Variable { name, ty, init, constness } => {
                (json!({
                    "name": {name.ident.sym.to_string()},
                    "type": {ty.map(|ty| ty.serialize(hir, sem)).unwrap_or(Json::Null)},
                    "init": {init.map(|init| init.serialize(hir, sem)).unwrap_or(Json::Null)},
                    "is_const": {matches!(constness, hir::Constness::Const)}
                }), "Variable")
            }
            hir::ItemKind::Function(function) => (function.serialize(hir, sem), "Function"),
            hir::ItemKind::Struct { name, fields } => {
                (json!({
                    "name": {name.ident.sym.to_string()},
                    "fields": {
                        Json::Array(fields.iter().map(|field| {
                            field.serialize(hir, sem)
                        }).collect())
                    }
                }), "Struct")
            },
            hir::ItemKind::Use(use_item) => (use_item.serialize(hir, sem), "Use"),
            hir::ItemKind::TypeAlias { ty, name } => (json!({
                "name": {name.ident.sym.to_string()},
                "ty": {ty.serialize(hir, sem)},
            }), "TypeAlias")
        };

        if let Some(obj) = obj.object_mut() {
            // TODO: expect
            obj.insert("kind".into(), kind.into());
        }
        obj
    }
}

impl JsonSerialize for hir::Type<'_> {
    fn serialize(&self, hir: &hir::Session<'_>, sem: &Semantic<'_>) -> Json {
        match &self.kind {
            hir::types::TypeKind::Primitive(primitive_type) => Json::String(match primitive_type {
                hir::types::PrimitiveType::I8 => "i8",
                hir::types::PrimitiveType::I16 => "i16",
                hir::types::PrimitiveType::I32 => "i32",
                hir::types::PrimitiveType::I64 => "i64",
                hir::types::PrimitiveType::U8 => "u8 ",
                hir::types::PrimitiveType::U16 => "u16",
                hir::types::PrimitiveType::U32 => "u32",
                hir::types::PrimitiveType::U64 => "u64",
                hir::types::PrimitiveType::Char => "cha",
                hir::types::PrimitiveType::F32 => "f32",
                hir::types::PrimitiveType::F64 => "f64",
                hir::types::PrimitiveType::Bool => "bool"
            }.into()),
            hir::types::TypeKind::Ref(to) => json!({
                "kind": "Ref",
                "to": {to.serialize(hir, sem)},
            }),
            hir::types::TypeKind::Array(of, len) => json!({
                "kind": "Array",
                "of": {of.serialize(hir, sem)},
                "len": {f64::from(*len)},
            }),
            hir::types::TypeKind::Tuple(items) => json!({
                "kind": "Tuple",
                "items": { Json::Array(items.iter().map(|it| it.serialize(hir, sem)).collect()) }
            }),
            hir::types::TypeKind::Path(path) => path.serialize(hir, sem),
            hir::types::TypeKind::Function { is_variadic, params, ret_ty } => json!({
                "is_variadic": {*is_variadic},
                "params": {Json::Array(params.iter().map(|p| p.serialize(hir, sem)).collect())},
                "ret_type": {ret_ty.serialize(hir, sem)},
            })
        }
    }
}

impl JsonSerialize for hir::Path {
    fn serialize(&self, _: &hir::Session<'_>, _: &Semantic<'_>) -> Json {
        json!({
            "kind": "Path",
            "segments": {Json::Array(self.segments.iter().map(|seg| Json::String(seg.ident.sym.to_string().into())).collect())}
        })
    }
}

impl JsonSerialize for hir::Expression<'_> {
    fn serialize(&self, hir: &hir::Session<'_>, sem: &Semantic<'_>) -> Json {
        Json::Null
    }
}

impl JsonSerialize for hir::Field<'_> {
    fn serialize(&self, hir: &hir::Session<'_>, sem: &Semantic<'_>) -> Json {
        json!({
            "name": {self.name.ident.sym.to_string()},
            "type": {self.ty.serialize(hir, sem)}
        })
    }
}
