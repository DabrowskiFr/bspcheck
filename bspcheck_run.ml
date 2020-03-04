let run () =
  let _ = PidDependent.compute () in
  let chan = open_out "cfg.dot" in
  let fmt = Format.formatter_of_out_channel chan in
  XCFG.output_cfg fmt


let () = Db.Main.extend run
