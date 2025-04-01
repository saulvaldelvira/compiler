use std::collections::HashMap;
use hir::HirId;

#[derive(Debug, Clone, Copy)]
pub enum MemoryAddress {
    Absolute(u16),
    Relative(i32),
}

#[derive(Clone)]
pub struct FunctionCtx {
    pub ret_size: u16,
    pub locals_size: u16,
    pub params_size: u16,
}

pub struct CodeGenerator<'cg> {
    addresses: HashMap<HirId, MemoryAddress>,
    functions: Vec<FunctionCtx>,
    global_offset: usize,
    labels: usize,
    source: &'cg str,
}

impl<'cg> CodeGenerator<'cg> {
    pub fn new(source: &'cg str) -> Self {
        Self {
            addresses: Default::default(),
            labels: Default::default(),
            functions: Vec::new(),
            global_offset: 0,
            source,
        }
    }

    pub fn is_global(&self) -> bool { self.functions.is_empty() }

    pub fn enter_function(&mut self, f: FunctionCtx) {
        self.functions.push(f);
    }

    pub fn exit_function(&mut self) {
        self.functions.pop();
    }

    pub fn current_function(&mut self) -> Option<FunctionCtx> {
        self.functions.last().cloned()
    }

    pub fn next_global_offset(&mut self, size: usize) -> MemoryAddress {
        let addr = MemoryAddress::Absolute(self.global_offset as u16);
        self.global_offset += size;
        addr
    }

    pub fn set_address(&mut self, hid: HirId, addr: MemoryAddress) {
        debug_assert!(!self.addresses.contains_key(&hid));
        self.addresses.insert(hid, addr);
    }

    pub fn address_of(&self, hid: &HirId) -> Option<MemoryAddress> {
        self.addresses.get(hid).copied()
    }

    pub fn next_label(&mut self) -> String {
        self.labels += 1;
        format!("label_{}", self.labels)
    }

    pub fn source(&self) -> &str { self.source }
}
