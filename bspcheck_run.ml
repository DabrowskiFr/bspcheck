let help_msg = "output a warm welcome message to the user"

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

open Cil_types
open Dataflows
open PdgTypes

(* Cil_types.stmtkind *)
let print_stmt out = function
    Instr i -> Printer.pp_instr out i
  |Return _ -> Format.pp_print_string out "<return>"
  |Goto _ -> Format.pp_print_string out "<goto>"
  |Break _ -> Format.pp_print_string out "<break>"
  |Continue _ -> Format.pp_print_string out "<continue>"
  |If(e,_,_,_) -> Format.fprintf out "if %a" Printer.pp_exp e
  |Switch(e,_,_,_) -> Format.fprintf out "switch %a" Printer.pp_exp e
  |Loop _ -> Format.fprintf out "<loop>"
  |Block _ -> Format.fprintf out "<block>"
  |UnspecifiedSequence _ -> Format.fprintf out "<unspecified sequence>"
  |TryFinally _
  |TryExcept _
  |TryCatch _ -> Format.fprintf out "<try>"
  |Throw _ -> Format.fprintf out "<throw>"

class print_cfg out =
  object
    inherit Visitor.frama_c_inplace
    method !vfile _ =
      Format.fprintf out "@[<hov 2>digraph cfg {@ ";
      Cil.DoChildrenPost(fun f -> Format.fprintf out "}@]@.";f)
  
    method !vglob_aux g =
      match g with
      |GFun(f,_) ->
        Format.fprintf out
          "@[<hov 2>subgraph cluster_%a {@ @[<hv 2>graph@ [label=\"%a\"];@]@ "
          Printer.pp_varinfo f.svar Printer.pp_varinfo f.svar;
        Cil.DoChildrenPost(fun g -> Format.fprintf out "}@]@ ";g)
      |_ -> Cil.SkipChildren

    method !vstmt_aux s =
      Format.fprintf out "@[<hov 2>s%d@ [label=%S]@];@ " s.sid
        (Pretty_utils.to_string print_stmt s.skind);
      List.iter(fun succ->Format.fprintf out"@[s%d -> s%d;@]@ "s.sid succ.sid)s.succs;
      Format.fprintf out"@]";
      Cil.DoChildren
    
end


let run() =
  let chan=open_out"cfg.out" in
  let fmt=Format.formatter_of_out_channel chan in
  Visitor.visitFramacFileSameGlobals (new print_cfg fmt) (Ast.get());
  close_out chan

let run2 () =
  let chan=open_out"dpg.out" in
  let fmt=Format.formatter_of_out_channel chan in
  let kf =  Globals.Functions.find_def_by_name "f" in
  let pdg = !Db.Pdg.get kf in
  let entrynode = !Db.Pdg.find_entry_point_node pdg in
  let () = !Db.Pdg.pretty_node true fmt entrynode in
  (*let () = !Db.Pdg.pretty fmt pdg in*)
  (*let u = !Db.Pdg.iter_nodes (!Db.Pdg.pretty_node true fmt) pdg in*)
  ()

(* Db.Pdg.get must be called before find calls *)
(* find all call from a function *)

(* Returns the list of call stmt to a pid dependent function in kf_f *)
  
let find_pid_dep_calls (kf_f : kernel_function) : Cil_types.stmt list =
  !Db.Pdg.find_call_stmts ~caller:kf_f (Globals.Functions.find_by_name "pid")

let is_control (s : Cil_types.stmtkind) : bool =
  match s with
  | If(_,_,_,_) -> true
  | _ -> false

let pretty_control (pdg : Db.Pdg.t) (s : Cil_types.stmt) out : unit =
  match s.skind with
  | If(_,_,_,_) ->
    match !Db.Pdg.find_simple_stmt_nodes pdg s with
      [n] ->
      Format.fprintf out "\ncontrol node %a if(%a) [from statement %a]"
        (!Db.Pdg.pretty_node true) n (!Db.Pdg.pretty_key) (!Db.Pdg.node_key n) (print_stmt) (s.skind)
  | _ -> failwith "not a control node"

class compute_pid_dep out = 
  object(self)
    inherit Visitor.frama_c_inplace

    method !vfile _ =
      Format.fprintf out "\n***in file***\n";
      Cil.DoChildrenPost (fun f -> f)
  
    method !vglob_aux g =
      match g with
      |GFun(f,_) ->
        (* let pdg = !Db.Pdg.get (Globals.Functions.get f.svar)  in (* the pdg must computed for other methods to succeed*) *)
        let pidcalls = find_pid_dep_calls (Globals.Functions.get f.svar) in
        Format.fprintf out "\n*********IN FUNCTION %a***\n with direct pid calls\n" Printer.pp_varinfo f.svar;
        List.iter (fun s -> Format.fprintf Format.std_formatter "\t%a\n" print_stmt s.skind) pidcalls;
        Cil.DoChildrenPost (fun g -> g)
      |_ -> Cil.SkipChildren

    method !vstmt_aux s =
      if is_control s.skind then
        match self#current_kf with
          None -> exit(-1)
        | Some kf_f ->
          let pdg = !Db.Pdg.get kf_f  in
          let node = !Db.Pdg.find_simple_stmt_nodes pdg s in
          pretty_control pdg s out;
          Format.fprintf out "\n********DEPENDS ON : ********\n";
          let l = !Db.Pdg.all_dpds pdg node in
          (List.iter (!Db.Pdg.pretty_node true Format.std_formatter)
             l);
          Format.fprintf out "\n";
        ;Cil.DoChildren
      else Cil.DoChildren
end

let run3 () =
  (* let chan=open_out"dpg.out" in *)
  (* let fmt = Format.formatter_of_out_channel chan in *)
  (* let kf =  Globals.Functions.find_def_by_name "f" in *)
  (* let pdg = !Db.Pdg.get kf in *)
  (* let pidcalls = find_pid_dep_calls kf in *)
  (* let t = List.hd pidcalls in *)
  (* let nodecall = !Db.Pdg.find_simple_stmt_nodes pdg t in *)
  (*let dependencies = !Db.Pdg.all_data_dpds pdg  nodecalls in*)
  (*List.iter (function s ->
      Format.fprintf Format.std_formatter "%i : %s\n"
        s.sid (Pretty_utils.to_string print_stmt s.skind)
    ) pidcalls
    ;*)
  (* let output = !Db.Pdg.find_call_output_node pdg t in
   * let dependencies = !Db.Pdg.all_dpds pdg [output] in *)

  
  Visitor.visitFramacFileSameGlobals (new compute_pid_dep Format.std_formatter)
    (Ast.get())
  
    
let runold() = 
  try
    if Enabled.get() then
      let filename=Output_file.get() in
      let output msg =
        if Output_file.is_default() then
          Self.result "%s" msg
        else
          let chan=open_out filename
          in
          Printf.fprintf  chan "%s\n" msg;
          flush chan;
          close_out chan;
      in output "Hello, world!"
  with Sys_error _ as exc ->
    let msg = Printexc.to_string exc
    in Printf.eprintf "There was an error: %s\n" msg

let () =Db.Main.extend run3

(* TODO *)
(* For each nodes, extract nodes on which it depends*)
(* Checks if there is an call to pid in them *)

(* Find nodes which are call to pid *)
(* Find control nodes depending on pid *)
(* Find all nodes control dependent on such nodes *)
(* using the visitor for each pass *)
