open Cil_types
open PdgTypes
let help_msg = "computes pid-dependent nodes of a bsp program"

module Self = Plugin.Register
    (struct
      let name= "BSP Checker"
      let shortname= "bspcheck"
      let help=help_msg
    end)

module Enabled = Self.False
    (struct
      let option_name= "-bspcheck"
      let help= "when on (off by default), " ^help_msg
    end)

module Output_file = Self.String
    (struct
      let option_name= "-bspcheck-output"
      let default= "-"
      let arg_name= "output-file"
      let help="file where the message is output (default: output to the console)"
    end)

let initial_functions = ["bsp_pid"]

class compute_pdgs out =
  object(self)
    inherit Visitor.frama_c_inplace

    val pdgs = Hashtbl.create 50

    method !vfile _ =
      Cil.DoChildrenPost (fun f -> f)

    method !vglob_aux g =
      match g with
      | GFun(f,_) ->
        (* computing the CFGs is needed to fill the sid field in statements *)
        Cfg.prepareCFG f;
        (* pre-compute all pdgs graphs and store them in a hashtable *)
        Hashtbl.add pdgs f.svar.vname (!Db.Pdg.get (Globals.Functions.get f.svar));
        Cil.DoChildren
        (* Cil.SkipChildren *)
      | _ -> Cil.SkipChildren

    method pdgs () = pdgs
  end

let run () =
  let chan = open_out "cfg.dot" in
  let fmt = Format.formatter_of_out_channel chan in
  let ast = Ast.get() in
  let outnodes = Hashtbl.create 50 in
  Hashtbl.add outnodes "pid" [(match PdgIndex.Key.output_key with SigKey s -> s)];
  let o = new compute_pdgs Format.std_formatter in
  Visitor.visitFramacFileSameGlobals (o : compute_pdgs :> Visitor.frama_c_inplace) ast;
  let r = ref true in
  while (!r) do
    let o2 = new PidDependent.compute_output_dep Format.std_formatter (o#pdgs ()) outnodes in
    Visitor.visitFramacFileSameGlobals (o2 : PidDependent.compute_output_dep :> Visitor.frama_c_inplace)
      ast;
    r := o2#continue ()
  done;
  let o' = new PidDependent.compute_pid_dep Format.std_formatter (o#pdgs ()) in 
  Visitor.visitFramacFileSameGlobals (o' : PidDependent.compute_pid_dep :> Visitor.frama_c_inplace) ast;
  Visitor.visitFramacFileSameGlobals (new XCFG.print_cfg fmt (o'#sids ())) ast;
  close_out chan


let () = Db.Main.extend run


