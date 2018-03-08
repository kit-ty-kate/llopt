let ctxt = Llvm.global_context ()
let init = lazy (Llvm_all_backends.initialize ())

let get_triple () =
  Lazy.force init;
  Llvm_target.Target.default_triple ()

let get_target ~triple =
  let target = Llvm_target.Target.by_triple triple in
  let reloc_mode = Llvm_target.RelocMode.PIC in
  Llvm_target.TargetMachine.create ~triple ~reloc_mode target

let optimize ~level ~lto m =
  let triple = get_triple () in
  let target = get_target ~triple in
  let layout = Llvm_target.TargetMachine.data_layout target in
  let layout = Llvm_target.DataLayout.as_string layout in
  Llvm.set_target_triple triple m;
  Llvm.set_data_layout layout m;
  let pm = Llvm.PassManager.create () in
  let pm_f = Llvm.PassManager.create_function m in (* TODO: Is this useful ? *)
  Llvm_target.TargetMachine.add_analysis_passes pm target; (* TODO Is this useful ? *)
  let b = Llvm_passmgr_builder.create () in
  Llvm_passmgr_builder.set_opt_level level b;
  Llvm_passmgr_builder.populate_module_pass_manager pm b;
  Llvm_passmgr_builder.populate_function_pass_manager pm_f b; (* TODO Is this useful ? *)
  if lto then begin
    Llvm_passmgr_builder.populate_lto_pass_manager
      ~internalize:true
      ~run_inliner:true
      pm
      b;
  end;
  ignore (Llvm.PassManager.run_module m pm);
  Llvm.PassManager.dispose pm

let () = match Sys.argv with
  | [|_; file|] ->
      let buf = Llvm.MemoryBuffer.of_file file in
      let m = Llvm_irreader.parse_ir ctxt buf in
      optimize ~level:3 ~lto:true m;
      Llvm.dump_module m;
  | _ ->
      assert false
