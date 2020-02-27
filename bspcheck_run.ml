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

open Cil_types
open Dataflows
open PdgTypes

let sids = Hashtbl.create 50



module Int = struct
  type t = int
  let compare = compare
end

module Ints = Set.Make(Int)

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
  object(self)
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
      match self#current_kf with
        None ->failwith "not in a function"
      | Some kf_f ->
        let id = (Globals.Functions.get_vi kf_f).vname in
        if List.mem s.sid (Hashtbl.find sids id) then
          begin
            Format.fprintf out "@[<hov 2>s%d@ [style=filled,fillcolor=lightsalmon,label=%S]@];@ " s.sid
              (Pretty_utils.to_string print_stmt s.skind);
            List.iter(fun succ->Format.fprintf out"@[s%d -> s%d;@]@ "s.sid succ.sid)s.succs;
            Format.fprintf out"@]";
            Cil.DoChildren
          end
        else
          begin
            Format.fprintf out "@[<hov 2>s%d@ [style=filled,fillcolor=lightblue,label=%S]@];@ " s.sid
              (Pretty_utils.to_string print_stmt s.skind);
            List.iter(fun succ->Format.fprintf out"@[s%d -> s%d;@]@ "s.sid succ.sid)s.succs;
            Format.fprintf out"@]";
            Cil.DoChildren
          end
  end

let find_pid_dep_calls (kf_f : kernel_function) : Cil_types.stmt list =
  !Db.Pdg.find_call_stmts ~caller:kf_f (Globals.Functions.find_by_name "pid")

let pretty_full_node out n = 
  Format.fprintf out "\n%a // %a\n" (!Db.Pdg.pretty_node true) n !Db.Pdg.pretty_key (!Db.Pdg.node_key n)

let print_line out () =
  Format.fprintf out "    ---------------------------------------------------------\n"

let print_in_function out (f : Cil_types.fundec) =
  Format.fprintf out "%a    In function %a\n%a\n" print_line () Printer.pp_varinfo f.svar print_line ()

let pdgs = Hashtbl.create 50

class compute_pdgs out =
  object(self)
    inherit Visitor.frama_c_inplace

    method !vfile _ =
      Cil.DoChildrenPost (fun f -> f)

    method !vglob_aux g =
      match g with
      | GFun(f,_) ->
        Cfg.prepareCFG f;
        let pdg = !Db.Pdg.get (Globals.Functions.get f.svar)  in
        Hashtbl.add pdgs f.svar.vname pdg;
        Cil.SkipChildren
      | _ -> Cil.SkipChildren
  end
    

let pid_dependent_statements = Hashtbl.create 50



let rec statements_of_nodes (l : PdgTypes.Node.t list) : Ints.t =
  begin match l with
      [] -> Ints.empty
    | h::t ->
      begin match (PdgTypes.Node.stmt h) with
          None -> statements_of_nodes t
        | Some s -> Ints.union (Ints.singleton s.sid) (statements_of_nodes t)
      end
  end

class compute_pid_dep out = 
  object(self)
    inherit Visitor.frama_c_inplace

    val pid_calls_list = Hashtbl.create 50
    
    method !vfile _ =
      Cil.DoChildrenPost (fun f -> f)
  
    method !vglob_aux g =
      match g with
      |GFun(f,_) ->
        let id = f.svar.vname in
        let pdg = Hashtbl.find pdgs id in
        let pidcalls = find_pid_dep_calls (Globals.Functions.get f.svar) in
        let pidcalls_nodes = List.map (!Db.Pdg.find_call_output_node pdg) pidcalls in
        Hashtbl.add pid_calls_list f.svar.vname pidcalls_nodes;
        Format.fprintf out "%a" print_in_function f;
        Format.fprintf out "    Pid dependent values are produced at nodes : "; 
        List.iter (fun s -> Format.fprintf Format.std_formatter "%a " (!Db.Pdg.pretty_node true) s) pidcalls_nodes;
        Format.fprintf out "\n";
        Hashtbl.add pdgs f.svar.vname pdg;
        Cil.DoChildrenPost (fun g ->
            let l = statements_of_nodes (Hashtbl.find_all pid_dependent_statements id) in
            Hashtbl.add sids id (Ints.elements l);
            (* List.iter (Format.fprintf out "\nchoosen %a " (!Db.Pdg.pretty_node true)) (Hashtbl.find_all pid_dependent_statements id); *)
            g )
      |_ -> Cil.SkipChildren

    method !vstmt_aux s =
      match s.skind with
        If(_,_,_,_) ->
        begin
          match self#current_kf with
            None -> exit(-1)
          | Some kf_f ->
            let id = (Globals.Functions.get_vi kf_f).vname in
            let pdg = Hashtbl.find pdgs id in
            let node = !Db.Pdg.find_stmt_node pdg s in
            Format.fprintf out "\n    node %a (%a) is data dependent on nodes\n\t"
              (!Db.Pdg.pretty_node true) node (print_stmt) (s.skind);
            let data_dp_on = !Db.Pdg.all_data_dpds pdg [node] in
            (List.iter (fun n -> Format.fprintf Format.std_formatter "%a " (!Db.Pdg.pretty_node true) n) data_dp_on);
            let pid_dp_on = ref false in
            List.iter (fun n -> if List.mem n (Hashtbl.find pid_calls_list id) then pid_dp_on := true) data_dp_on;
            Format.fprintf out "\n    The nodes is pid dependend : %b\n" !pid_dp_on;
            Format.fprintf out "    The following nodes are control dependent on node %a\n\t" (!Db.Pdg.pretty_node true) node;
            let m = !Db.Pdg.direct_ctrl_uses pdg node in
            (List.iter (fun n -> Format.fprintf Format.std_formatter "%a " (!Db.Pdg.pretty_node true) n) m);
            if !pid_dp_on then List.iter (Hashtbl.add pid_dependent_statements id) m;
            Format.fprintf out "\n";
            ;Cil.DoChildren
        end
      | Instr (Call (_,_,_,_)) ->
        begin
          match self#current_kf  with
            Some kf_f ->
            (* let pdg = Hashtbl.find pdgs (Globals.Functions.get_vi kf_f).vname in *)
            (* let nodes = !Db.Pdg.find_simple_stmt_nodes pdg s in *)
            (* Format.fprintf out "\n******** CALL *********\n"; (print_stmt out s.skind); *)
            (* List.iter (pretty_full_node out) nodes; *)
            (* let outret_node = !Db.Pdg.find_call_output_node pdg s in *)
            (* let outret = PdgIndex.Key.call_outret_key s in *)
            (* Format.fprintf out "\nuuu output ret %a \n" (!Db.Pdg.pretty_node true) (outret_node);
             * Format.fprintf out "\n output ret %a \n" !Db.Pdg.pretty_key (PdgIndex.Key.call_outret_key s);
             * Format.fprintf out "\n***********************\n"; *)
            Cil.DoChildren 
          | None -> failwith "not in a function"
        end
      | _ -> Cil.DoChildren
  end

let print_ints out l =
  List.iter (fun n -> Format.fprintf out "%i " n) l

let run () =
  let chan = open_out "cfg.dot" in
  let fmt = Format.formatter_of_out_channel chan in
  let ast = Ast.get() in
  Visitor.visitFramacFileSameGlobals
    (new compute_pdgs Format.std_formatter)
    ast;
  Visitor.visitFramacFileSameGlobals
    (new compute_pid_dep Format.std_formatter)
    ast;
  Hashtbl.iter (fun f s -> Format.fprintf Format.std_formatter "%s %a\n" f print_ints s) sids;
  Visitor.visitFramacFileSameGlobals
    (new print_cfg fmt)
    ast;
  close_out chan
(* Hashtbl.iter (fun f s -> Format.fprintf Format.std_formatter "%s %i" f (List.length s)) sids *)

let () =Db.Main.extend run

(* TODO *)
(* For each nodes, extract nodes on which it depends*)
(* Checks if there is an call to pid in them *)

(* Find nodes which are call to pid *)
(* Find control nodes depending on pid *)
(* Find all nodes control dependent on such nodes *)
(* using the visitor for each pass *)
