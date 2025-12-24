use std::collections::HashMap;

use hir::HirId;
use semantic::Semantic;
use span::source::SourceMap;

#[derive(Debug, Clone, Copy)]
pub enum MemoryAddress {
    Absolute(u16),
    Relative(i32),
}

#[derive(Clone)]
#[expect(clippy::struct_field_names)]
pub struct FunctionCtx {
    pub ret_size: u16,
    pub locals_size: u16,
    pub params_size: u16,
}

pub struct CodeGenerator<'cg, 'sem, 'hir> {
    addresses: HashMap<HirId, MemoryAddress>,
    functions: Vec<FunctionCtx>,
    global_offset: u64,
    labels: usize,
    mangle_prefix: Vec<String>,
    mangles: HashMap<HirId, String>,

    pub source: &'cg SourceMap,
    pub sem: &'cg Semantic<'sem>,
    pub hir: &'cg hir::Session<'hir>,
}

impl<'cg, 'sem, 'hir> CodeGenerator<'cg, 'sem, 'hir> {
    pub fn new(
        source: &'cg SourceMap,
        semantic: &'cg Semantic<'sem>,
        hir: &'cg hir::Session<'hir>,
    ) -> Self {
        Self {
            addresses: HashMap::default(),
            labels: Default::default(),
            functions: Vec::new(),
            global_offset: 0,
            source,
            mangle_prefix: vec![],
            mangles: HashMap::new(),
            sem: semantic,
            hir,
        }
    }

    pub fn is_global(&self) -> bool { self.functions.is_empty() }

    pub fn enter_function(&mut self, f: FunctionCtx) { self.functions.push(f); }

    pub fn exit_function(&mut self) { self.functions.pop(); }

    pub fn current_function(&mut self) -> Option<FunctionCtx> { self.functions.last().cloned() }

    pub fn next_global_offset(&mut self, size: u64) -> MemoryAddress {
        #[expect(clippy::cast_possible_truncation)]
        let addr = MemoryAddress::Absolute(self.global_offset as u16);
        self.global_offset += size;
        addr
    }

    pub fn mangle_symbol(&mut self, id: HirId, sym: &str) {
        let mut name = String::new();
        for prefix in &self.mangle_prefix {
            name.push_str(prefix);
            name.push('_');
        }
        name.push_str(sym);
        self.mangles.insert(id, name);
    }

    pub fn get_mangled_symbol(&self, id: HirId) -> Option<String> { self.mangles.get(&id).cloned() }

    pub fn enter_module(&mut self, name: String) { self.mangle_prefix.push(name); }

    pub fn exit_module(&mut self) { self.mangle_prefix.pop(); }

    pub fn set_address(&mut self, hid: HirId, addr: MemoryAddress) {
        debug_assert!(!self.addresses.contains_key(&hid));
        self.addresses.insert(hid, addr);
    }

    pub fn address_of(&self, hid: HirId) -> Option<MemoryAddress> {
        self.addresses.get(&hid).copied()
    }

    pub fn next_label(&mut self) -> String {
        self.labels += 1;
        format!("label_{}", self.labels)
    }
}
