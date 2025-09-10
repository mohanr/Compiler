open Llvm_analysis
open Codegen
let _ =
  let llctx = Llvm.global_context () in
  Llvm.MemoryBuffer.of_file Sys.argv.(1);
  (* let llm = Llvm_bitreader.parse_bitcode llctx llmem in *)
  (* Llvm.dump_module llm ; *)
  (* () *)
