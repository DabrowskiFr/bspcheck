open Cil_types


module Int = struct
  type t = int
  let compare = compare
end

module Ints = Set.Make(Int)


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

let sigKeyofSigCallKey s = 
  match !Db.Pdg.node_key s with
  SigKey s -> s
  | SigCallKey (_,s) -> s | _ -> failwith "invalid key"

let extract
    (pdg : PdgTypes.Pdg.t) ((stmt,keys) : Cil_types.stmt * PdgIndex.Signature.key list ) :
  PdgTypes.Node.t list = 
  let nodes = !Db.Pdg.find_simple_stmt_nodes pdg stmt in
  List.filter (fun n -> List.mem (sigKeyofSigCallKey n) keys) nodes

let rec list_dist l1 l2 =
  match l1 with
    [] -> []
  | h::t -> (h,l2)::(list_dist t l2)


let print_output_keys out keys id = 
  Aux.print_line out ();
  Format.fprintf out ("\n    Pid dependent output nodes in %s\n") id;
  List.iter (fun n ->
      Format.fprintf out "    %a\n" 
        PdgIndex.Signature.pretty_key n
    ) keys;
  Format.fprintf out "\n"

(* compute output nodes of calls producing pid-dependent values (as stated by outnodes) *)
let get_pid_dep_call_nodes pdg id outnodes = 
  let stmts_keys =
    let kf_f = Globals.Functions.find_by_name id in
    Hashtbl.fold
      (fun g  keys t ->
         let kf_g = Globals.Functions.find_by_name g in
         (List.map
            (fun stmt -> (stmt, keys)) (!Db.Pdg.find_call_stmts ~caller:kf_f kf_g)) @ t)
      outnodes []
  in List.flatten (List.map (extract pdg) (stmts_keys))
  
class compute_output_dep out pdgs (outnodes : (string, PdgIndex.Signature.key list) Hashtbl.t) =
  object(self)
    inherit Visitor.frama_c_inplace

    val newoutnodes = Hashtbl.create 50

    val mutable modified = false
    
    method !vfile _ =
      Cil.DoChildrenPost (fun f ->
          Hashtbl.iter (fun x y -> Hashtbl.replace outnodes x ((try Hashtbl.find outnodes x with _ -> [])@y))
            newoutnodes;f)
        
    method !vglob_aux g =
      match g with
      |GFun(f,_) ->
        let id = f.svar.vname in
        let pdg = Hashtbl.find pdgs id in
        begin
          try  
            let output_nodes = [!Db.Pdg.find_ret_output_node pdg] in
            let inputs = !Db.Pdg.find_all_inputs_nodes pdg  in
            (* List.iter (fun n -> Format.fprintf out "%a \n" (!Db.Pdg.pretty_key) (!Db.Pdg.node_key n)) inputs; *)
            (* compute output nodes keys of the current function that depend on nodes in pid_dep_call_nodes*)
            let pid_dep_output_keys =
              let pid_dep_call_nodes = get_pid_dep_call_nodes pdg id outnodes in
              List.map sigKeyofSigCallKey
                (List.filter
                   (fun n ->
                      let alldeps = !Db.Pdg.all_dpds pdg [n] in
                      List.exists (fun h -> List.mem h pid_dep_call_nodes) alldeps)
                   output_nodes) in
            let pid_dep_input_keys =
              let pid_dep_call_nodes = get_pid_dep_call_nodes pdg id outnodes in
              let calls' = List.flatten  (List.map (!Db.Pdg.direct_data_uses pdg) pid_dep_call_nodes) in
              let calls = List.flatten ( List.map (!Db.Pdg.direct_addr_dpds pdg) (pid_dep_call_nodes @ calls')) in
              List.map sigKeyofSigCallKey
                (List.filter (fun i ->
                     (* (Format.fprintf out "\nXXX %a\n" !Db.Pdg.pretty_key (!Db.Pdg.node_key i))  ; *)
                     List.mem i (calls@calls') ) inputs) in
            (* record discovered keys ans set modified to true if a new turn is needed *)
            let oldkeys = try Hashtbl.find outnodes id with _ -> [] in
            let newkeys =
              List.fold_right (fun k r -> if not (List.mem k oldkeys) then k::r else r) (pid_dep_output_keys@pid_dep_input_keys) [] in
            if newkeys <> [] then
              begin
                modified <- true;
                Hashtbl.add newoutnodes id newkeys
              end;
            (* compute output locations keys of the current function that depend on nodes in pid_dep_call_nodes *)
            print_output_keys out (pid_dep_output_keys@pid_dep_input_keys) id;
            (* print_output_keys out (pid_dep_output_keys@pid_dep_input_keys) id; *)
            Cil.SkipChildren
          with _ -> Format.fprintf out "\nNot handled %s\n" id; Cil.SkipChildren
        end
      | _ -> Cil.SkipChildren

    method continue () = modified
  end

let find_pid_dep_calls (kf_f : kernel_function) : Cil_types.stmt list =
  !Db.Pdg.find_call_stmts ~caller:kf_f (Globals.Functions.find_by_name "pid")


class compute_pid_dep out pdgs outnodes = 
  object(self)
    inherit Visitor.frama_c_inplace

    val sids = Hashtbl.create 50
    
    val pid_calls_list = Hashtbl.create 50
    
    method !vfile _ =
      Cil.DoChildrenPost (fun f -> f)
  
    method !vglob_aux g =
      match g with
      |GFun(f,_) ->
        let id = f.svar.vname in
        let pdg = Hashtbl.find pdgs id in
        let pidcalls_nodes = get_pid_dep_call_nodes pdg id outnodes in
        Hashtbl.add pid_calls_list f.svar.vname pidcalls_nodes;
        Format.fprintf out "%a" Aux.print_in_function f;
        Format.fprintf out "    Pid dependent values are produced at nodes : "; 
        List.iter (fun s -> Format.fprintf Format.std_formatter "%a " (!Db.Pdg.pretty_node true) s)
          pidcalls_nodes;
        Format.fprintf out "\n";
        Hashtbl.add pdgs f.svar.vname pdg;
        Cil.DoChildrenPost (fun g ->
            let l = statements_of_nodes (Hashtbl.find_all pid_dependent_statements id) in
            Hashtbl.add sids id (Ints.elements l);
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
              (!Db.Pdg.pretty_node true) node (Aux.print_stmt) (s.skind);
            let data_dp_on = !Db.Pdg.all_data_dpds pdg [node] in
            (List.iter (fun n -> Format.fprintf Format.std_formatter "%a " (!Db.Pdg.pretty_node true) n)
               data_dp_on);
            let pid_dp_on = ref false in
            List.iter (fun n -> if List.mem n (Hashtbl.find pid_calls_list id) then pid_dp_on := true)
              data_dp_on;
            Format.fprintf out "    The following nodes are control dependent on node %a\n\t"
              (!Db.Pdg.pretty_node true) node;
            let m = !Db.Pdg.direct_ctrl_uses pdg node in
            (List.iter (fun n -> Format.fprintf Format.std_formatter "%a " (!Db.Pdg.pretty_node true) n) m);
            if !pid_dp_on then List.iter (Hashtbl.add pid_dependent_statements id) m;
            Format.fprintf out "\n";
            ;Cil.DoChildren
        end
      | Instr (Call (_,_,_,_)) ->
        begin
          match self#current_kf  with
            Some kf_f -> Cil.DoChildren 
          | None -> failwith "not in a function"
        end
      | _ -> Cil.DoChildren

    method sids () = sids
  end
