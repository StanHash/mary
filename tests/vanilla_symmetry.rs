#[cfg(feature = "test_with_roms")]
mod tests {
    use std::{env, fs, io};

    use mary::compiler::{self, error::CompileErrors, ScriptError};
    use thiserror::Error;

    #[derive(Debug, Error)]
    enum TestFailure {
        #[error("IO Error")]
        IoError(#[from] io::Error),

        #[error("Environment variable error")]
        EnvVarError(#[from] env::VarError),

        #[error("Test failed on script {0}")]
        ScriptFailed(usize),

        #[error("Lib script parse failed: {0}")]
        LibScriptError(ScriptError),

        #[error("Not all scripts could be decompiled or recompiled")]
        DecompileFailure,
    }

    fn reencode_scripts(which_rom: &str) -> Result<(), TestFailure> {
        use mary::{bytecode, utility::rom_info};

        let rom = fs::read(env::var(which_rom)?)?;
        let encoded_scripts = rom_info::get_all_scripts(&rom)?;

        for i in 0..encoded_scripts.len() {
            let to_decode = encoded_scripts[i];
            let script = bytecode::decode_script(&mut &to_decode[..]).unwrap();
            let reencoded = bytecode::encode_script(&script);

            if !(to_decode == reencoded) {
                fs::write(format!("test_failed_base_{0}.dmp", i + 1), to_decode)?;
                fs::write(format!("test_failed_reen_{0}.dmp", i + 1), reencoded)?;

                return Err(TestFailure::ScriptFailed(i + 1));
            }
        }

        Ok(())
    }

    #[test]
    fn script_reencode_fomt() -> Result<(), TestFailure> {
        /* FOMT US : 0x080F89D4 1328 */

        let which_rom = "FOMT_US_GBA";
        // let script_table_addr = 0x080F89D4;
        // let script_table_size = 1328;

        reencode_scripts(which_rom)
    }

    #[test]
    fn script_reencode_mfomt() -> Result<(), TestFailure> {
        /* MFOMT US : 0x081014BC 1415 */

        let which_rom = "MFOMT_US_GBA";
        // let script_table_addr = 0x081014BC;
        // let script_table_size = 1415;

        reencode_scripts(which_rom)
    }

    fn recompile_scripts(which_rom: &str, which_lib: &str) -> Result<(), TestFailure> {
        use mary::{
            bytecode::{decode_script, encode_script},
            compiler::compile_script,
            decompiler::decompile_script,
            decompiler::DecompileErrorExtra,
            utility::rom_info::get_all_scripts,
        };

        #[derive(Debug)]
        enum FailType<'a> {
            Ok,

            DecompileError(DecompileErrorExtra<'a>),
            RecompileError(CompileErrors),
            OutputMismatch(Vec<u8>),
        }

        let rom = fs::read(env::var(which_rom)?)?;
        let encoded_scripts = get_all_scripts(&rom)?;

        let lib_text = fs::read_to_string(env::var(which_lib)?)?;

        let lib_scope = match compiler::parse_string(&lib_text) {
            Ok(parse_context) => parse_context.const_scope,
            Err(err) => return Err(TestFailure::LibScriptError(err)),
        };

        let mut scripts = vec![];

        for i in 0..encoded_scripts.len() {
            let to_decode = encoded_scripts[i];
            let decoded = decode_script(&mut &to_decode[..]).unwrap();
            scripts.push(decoded);
        }

        let mut results = vec![];

        for i in 0..encoded_scripts.len() {
            let to_decode = encoded_scripts[i];
            let decoded = &scripts[i];

            let decompiled = match decompile_script(&decoded, &lib_scope) {
                Ok(decompiled) => decompiled,

                Err(err) => {
                    results.push(FailType::DecompileError(err));
                    continue;
                }
            };

            let recompiled = match compile_script(decompiled, &lib_scope) {
                Ok(script) => script,

                Err(err) => {
                    results.push(FailType::RecompileError(err));
                    continue;
                }
            };

            let reencoded = encode_script(&recompiled);

            if !(to_decode == reencoded) {
                results.push(FailType::OutputMismatch(reencoded));
                continue;
            }

            results.push(FailType::Ok);
        }

        let success_count = results.iter().filter(|r| matches!(r, FailType::Ok)).count();

        if success_count == scripts.len() {
            return Ok(());
        }

        let mut first = true;

        for i in 0..results.len() {
            let dump_this = first;
            let dump_message = if dump_this { " (dumped)" } else { "" };

            match &results[i] {
                FailType::Ok => continue,

                FailType::DecompileError(err) => {
                    if dump_this {
                        let error_message = format!("{err}\n{0}", err.state_at_error());
                        fs::write(format!("decompile_error_{0}.txt", i + 1), error_message)?;
                    }

                    println!("Script {0} decompile error: {err}{dump_message}", i + 1)
                }

                FailType::RecompileError(compile_errors) => {
                    if dump_this {
                        let error_message = format!("{compile_errors}");
                        fs::write(format!("decompile_error_{0}.txt", i + 1), error_message)?;
                    }

                    println!("Script {0} recompile error{dump_message}", i + 1)
                }

                FailType::OutputMismatch(bad_output) => {
                    if dump_this {
                        fs::write(format!("input_{0}.dmp", i + 1), encoded_scripts[i])?;
                        fs::write(format!("output_{0}.dmp", i + 1), bad_output)?;
                    }

                    println!("Script {0} output mismatch{dump_message}", i + 1)
                }
            }

            first = first && !dump_this;
        }

        let success_rate_of_10000 = success_count * 10000 / scripts.len();

        println!(
            "{0}.{1}% ({2}/{3}) decompilation success",
            success_rate_of_10000 / 100,
            success_rate_of_10000 % 100,
            success_count,
            scripts.len()
        );

        Err(TestFailure::DecompileFailure)
    }

    #[test]
    fn script_recompile_fomt() -> Result<(), TestFailure> {
        /* FOMT US : 0x080F89D4 1328 */

        let which_rom = "FOMT_US_GBA";
        let which_lib = "FOMT_US_MARY_LIB";

        // let script_table_addr = 0x080F89D4;
        // let script_table_size = 1328;

        recompile_scripts(which_rom, which_lib)
    }

    #[test]
    fn script_recompile_mfomt() -> Result<(), TestFailure> {
        /* MFOMT US : 0x081014BC 1415 */

        let which_rom = "MFOMT_US_GBA";
        let which_lib = "MFOMT_US_MARY_LIB";
        // let script_table_addr = 0x081014BC;
        // let script_table_size = 1415;

        recompile_scripts(which_rom, which_lib)
    }
}
