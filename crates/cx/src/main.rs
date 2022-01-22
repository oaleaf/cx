use cranelift::codegen::binemit::NullTrapSink;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use target_lexicon::{Architecture, BinaryFormat, Environment, OperatingSystem, Triple};

use cranelift::codegen::binemit::NullStackMapSink;

fn main() {
    let mut triple = Triple::unknown();
    if cfg!(all(unix, not(target_os = "macos"))) {
        triple.binary_format = BinaryFormat::Elf;
        triple.operating_system = OperatingSystem::Linux;
    } else {
        panic!("Unsupported os");
    }

    if cfg!(target_arch = "x86_64") {
        triple.architecture = Architecture::X86_64;
    } else {
        panic!("Unsupported architecture");
    }

    triple.environment = Environment::Gnu;

    println!("{}", triple);

    // TODO: Custom newline aware iterator in lalrpop.
    let s = cx_syn::parse(
        r#"
        {
            usn std
            usn std::x
            
            a
                .b
                .c
        }
        "#,
    );
    dbg!(&s);
    let lowered = cx_lower::lower(s);
    dbg!(&lowered);

    let builder = JITBuilder::new(cranelift_module::default_libcall_names());
    let mut jitmod = JITModule::new(builder);
    let mut ctx = jitmod.make_context();
    ctx.func.name = ExternalName::User {
        namespace: 0,
        index: 0,
    };
    ctx.func.signature.params.push(AbiParam::new(types::I64));
    ctx.func.signature.returns.push(AbiParam::new(types::I64));

    let mut fbc = FunctionBuilderContext::new();
    let mut fb = FunctionBuilder::new(&mut ctx.func, &mut fbc);
    let b = fb.create_block();

    fb.append_block_params_for_function_params(b);

    let v = fb.block_params(b)[0];

    fb.switch_to_block(b);

    let one_v = fb.ins().iconst(types::I64, 1_i64);
    let rtv = fb.ins().iadd(v, one_v);

    fb.ins().return_(&[rtv]);

    fb.seal_block(b);
    fb.finalize();

    println!("{}", fb.func.display());

    let func_id = jitmod
        .declare_function("add_one", Linkage::Export, &ctx.func.signature)
        .unwrap();
    jitmod
        .define_function(
            func_id,
            &mut ctx,
            &mut NullTrapSink {},
            &mut NullStackMapSink {},
        )
        .unwrap();

    jitmod.finalize_definitions();

    let finalized = jitmod.get_finalized_function(func_id);

    let i = 1_i64;

    let invocable = unsafe { std::mem::transmute::<_, extern "C" fn(i64) -> i64>(finalized) };
    let o = invocable(i);

    dbg!(i);
    dbg!(o);

    assert_eq!(i + 1, o);
}
