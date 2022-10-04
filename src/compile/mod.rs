use crate::ast::*;
use inkwell::*;
use miette::Result;

pub fn compile_module(mod_file: ModFile) -> Result<()> {
    let ctx = context::Context::create();
    let lmod = ctx.create_module(mod_file.module.name.as_str());
    let builder = ctx.create_builder();
    for top in mod_file.top_list {
        match top {
            Top::DefineVar(range, name, expr) => {
                // bring tyck info to here
                lmod.add_global(ctx.i64_type(), Some(AddressSpace::Const), name.as_str());
            }
            Top::DefineProc(range, name, p_list, body) => {
                // let fn_type = context.f32_type().fn_type(&[], false);
                let f = lmod.add_function(name.as_str(), ctx.i64_type().fn_type(&[], false), None);
                let bb = ctx.append_basic_block(f, "");
                builder.position_at_end(bb);
            }
            _ => {}
        }
    }
    Ok(())
}
