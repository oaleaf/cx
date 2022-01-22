use cx_ir::{CXContext, CXFunctionDecl, CXNamespaceItem, CXPrimitiveTy, CXTyBase, CXTyRepr};
use cx_syn::ast::{Item, SourceFileRoot};

pub fn lower(source: SourceFileRoot) -> CXContext {
    let mut ctx = CXContext::new();

    for item in source.items.iter() {
        match item {
            Item::UsnStmt(s) => {
                // let nrctx = &mut ctx.name_resolve_context;
            }
            Item::Fn(f) => {
                let name = ctx.string_interner.intern_name(f.name.name());
                let fd = CXFunctionDecl {
                    name,
                    params: vec![],
                    default_params: vec![],
                    return_ty: CXTyRepr {
                        core: CXTyBase::Primitive(CXPrimitiveTy::Unit),
                        sub: None,
                    },
                };
                ctx.namespace_context
                    .global_namespace
                    .items
                    .push(CXNamespaceItem::FunctionDecl(fd));
            }
            Item::Stmt(s) => {}

            Item::None => {}
        }
    }

    ctx
}
