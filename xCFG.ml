open Cil_types
open Dataflows
open PdgTypes

(* sids : (string, int list) Hashtbl.t maps function ids to lists of pid dependent statement ids *)

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
        let color = if List.mem s (PidDependent.get_pid_dep_statements kf_f) then "lightsalmon" else "lightblue" in
        Format.fprintf out "@[<hov 2>s%d@ [style=filled,fillcolor=%S,label=%S]@];@ " s.sid color
          (Pretty_utils.to_string Aux.print_stmt s.skind);
        List.iter(fun succ->Format.fprintf out"@[s%d -> s%d;@]@ "s.sid succ.sid) s.succs;
        Format.fprintf out"@]";
        Cil.DoChildren
  end

let output_cfg fmt =
  let ast = Ast.get () in
  Visitor.visitFramacFileSameGlobals (new print_cfg fmt)  ast
