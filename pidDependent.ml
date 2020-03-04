open Cil_types

module Int = struct
  type t = int
  let compare = compare
end

module Ints = Set.Make(Int)

  
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

let rec keep_sig l =
  match l with
  [] -> []
  | (PdgIndex.Key.SigKey s)::t -> s::(keep_sig t)
  | SigCallKey (_,s)::t -> s::(keep_sig t)
  | h::t -> (keep_sig t)
          

let extract
    (pdg : PdgTypes.Pdg.t) ((stmt, keys) : Cil_types.stmt * PdgIndex.Signature.key list ) : PdgTypes.Node.t list = 
  let nodes = !Db.Pdg.find_simple_stmt_nodes pdg stmt in
  let l = List.filter (fun n -> List.exists (fun m -> List.mem m keys) (keep_sig (List.map !Db.Pdg.node_key (!Db.Pdg.direct_data_dpds pdg n))))
      nodes in
  (List.filter (fun n -> List.mem (sigKeyofSigCallKey n) keys) nodes)@l
  
(* compute output nodes of calls producing pid-dependent values (as stated by outnodes) *)
let get_pid_dep_call_nodes pdg kf_f (outnodes : (Cil_types.kernel_function, PdgIndex.Signature.key list) Hashtbl.t) : PdgTypes.Node.t list = 
  let stmts_keys =
    Hashtbl.fold
      (fun kf_g  keys t ->         
         (List.map
            (fun stmt -> (stmt, keys)) (!Db.Pdg.find_call_stmts ~caller:kf_f kf_g)) @ t)
      outnodes []
  in List.flatten (List.map (extract pdg) (stmts_keys))

(* let pdgs = Hashtbl.create 50 *)

let print_dependent_sources out outnodes =
  Format.fprintf out "    Iteration ...\n";
  Hashtbl.iter (fun kf_g l ->
      Format.fprintf out "    %s [ " (Globals.Functions.get_vi kf_g).vname;
      List.iter (fun n -> Format.fprintf out "%a " PdgIndex.Signature.pretty_key n) l;
      Format.fprintf out " ] ";
    ) outnodes;
  Format.fprintf out "\n";

class compute_output_dep (outnodes : (Cil_types.kernel_function, PdgIndex.Signature.key list) Hashtbl.t) =
  object(self)
    inherit Visitor.frama_c_inplace

    val newoutnodes = Hashtbl.create 50

    val out = Format.std_formatter

    val mutable continue = true
    
    method !vfile f =
      continue <- false;
      (* Cfg.computeFileCFG f; *)
      print_dependent_sources out outnodes;
      Cil.DoChildrenPost (fun f ->
          Hashtbl.iter (fun x y -> Hashtbl.replace outnodes x ((try Hashtbl.find outnodes x with _ -> [])@y))
            newoutnodes;f)
        
    method !vglob_aux g =
      match g with
      |GFun(f,_) ->
        Bspcheck_options.Self.feedback "[bspcheck] computing dependent sources in %s\n" f.svar.vname;
        let id = f.svar.vname in
         let kf_f = Globals.Functions.get f.svar in
        let pdg = !Db.Pdg.get (Globals.Functions.get f.svar) in
        begin
          try  
            let output_nodes = [!Db.Pdg.find_ret_output_node pdg] in
            let inputs = !Db.Pdg.find_all_inputs_nodes pdg  in
            (* compute output nodes keys of the current function that depend on nodes in pid_dep_call_nodes*)
            let pid_dep_call_nodes = get_pid_dep_call_nodes pdg kf_f outnodes in
            (* List.iter (fun k -> Format.fprintf out "XXXX%a %a\n" (!Db.Pdg.pretty_node true) k !Db.Pdg.pretty_key (!Db.Pdg.node_key k)) pid_dep_call_nodes; *)
            let pid_dep_output_keys =
              List.map sigKeyofSigCallKey
                (List.filter
                   (fun n ->
                      let alldeps = !Db.Pdg.all_dpds pdg [n] in
                      List.exists (fun h -> List.mem h pid_dep_call_nodes) alldeps)
                   output_nodes) in
            let pid_dep_input_keys =
              let calls' = List.flatten  (List.map (!Db.Pdg.direct_data_uses pdg) pid_dep_call_nodes) in
              let calls = List.flatten ( List.map (!Db.Pdg.direct_addr_dpds pdg) (pid_dep_call_nodes @ calls')) in
              List.map sigKeyofSigCallKey
                (List.filter (fun i ->
                     List.mem i (calls@calls') ) inputs) in
            (* record discovered keys ans set modified to true if a new turn is needed *)
            let oldkeys = try Hashtbl.find outnodes kf_f with _ -> [] in
            let newkeys =
              List.fold_right (fun k r -> if not (List.mem k oldkeys) then k::r else r) (pid_dep_output_keys@pid_dep_input_keys) [] in
            if newkeys <> [] then
              begin
                continue <- true;
                Hashtbl.add newoutnodes kf_f newkeys
              end
                
          with _ -> Bspcheck_options.Self.error "\nNot handled %s\n" id;
        end ;
        Bspcheck_options.Self.feedback "[bspcheck] done computing dependent sources in %s\n" f.svar.vname;
        Cil.SkipChildren
      | _ -> Cil.SkipChildren

    method continue () = continue
  end

let print_ta out s node pid_ctrl_uses =
  Format.fprintf out "\n    node %a (%a) is data dependent on nodes\n"
    (!Db.Pdg.pretty_node true) node (Aux.print_stmt) (s.skind);
  (List.iter (fun n -> Format.fprintf Format.std_formatter "%a " (!Db.Pdg.pretty_node true) n)
     pid_ctrl_uses);
  Format.fprintf out "\n    The following nodes are control dependent on node %a\n"
    (!Db.Pdg.pretty_node true) node;
  (List.iter (fun n -> Format.fprintf Format.std_formatter "    %a in %a\n"
                 (!Db.Pdg.pretty_node true) n !Db.Pdg.pretty_key (!Db.Pdg.node_key n)) pid_ctrl_uses);
  Format.fprintf out "\n"

let sources = ["bsp_pid"]

let get_pid_dep_output_nodes () =
  let ast = Ast.get() in
  let r = ref true in
  let outnodes = Hashtbl.create 50 in
  let _ =
    List.iter (fun s -> Hashtbl.add outnodes (Globals.Functions.find_by_name s) [(match PdgIndex.Key.output_key with SigKey s -> s)]) sources in
  while (!r) do
    let o2 = new compute_output_dep outnodes in
    Visitor.visitFramacFileSameGlobals (o2 : compute_output_dep :> Visitor.frama_c_inplace)
      ast;
    r := o2#continue ()
  done;
  outnodes


(** Compute pid dependent statements *)
(** pid_depdendent_statements : (Cil_types.kernel_function, statement) Hashtbl.t *)

let pid_dependent_statements = Hashtbl.create 50

class compute_pid_dep = 
  object(self)
    inherit Visitor.frama_c_inplace

    val pid_calls_list = Hashtbl.create 50

    val out = Format.std_formatter

    val outnodes = get_pid_dep_output_nodes ()
    
    method !vfile _ =
      Hashtbl.reset pid_dependent_statements;
      Cil.DoChildrenPost (fun f -> f)

    method !vglob_aux g =
      match g with
      |GFun(f,_) ->
        Bspcheck_options.Self.feedback "computing pid dependent statements in %s\n" f.svar.vname;
        let kf_f = Globals.Functions.get f.svar in
        let pdg = !Db.Pdg.get kf_f in
        let pidcalls_nodes = get_pid_dep_call_nodes pdg kf_f outnodes in
        Hashtbl.add pid_calls_list kf_f pidcalls_nodes;
        
        Format.fprintf out "%a" Aux.print_in_function f;
        Format.fprintf out "    Pid dependent values are produced at nodes : "; 
        List.iter (fun s -> Format.fprintf Format.std_formatter "%a " (!Db.Pdg.pretty_node true) s)
          pidcalls_nodes;
        Format.fprintf out "\n";
        Cil.DoChildrenPost (fun h ->
            Bspcheck_options.Self.feedback "done computing pid dependent sources in %s\n" f.svar.vname ; h);
      |_ -> Cil.SkipChildren

    (* nodes on which the condition is data dependent *)
    (* are they pid dependent calls in data_dp_on*)
    (* if yes add nodes that are control dependent on the condition to the result *)
              
    method !vstmt_aux s =
      match s.skind with
        If(_,_,_,_) ->
        begin
          match self#current_kf with
            None -> exit(-1)
          | Some kf_f ->
            let pdg = !Db.Pdg.get kf_f in
            let node = !Db.Pdg.find_stmt_node pdg s in
            let pid_data_dpds = !Db.Pdg.all_data_dpds pdg [node] in
            let is_pid_data_deps = List.exists (Fun.flip List.mem (Hashtbl.find pid_calls_list kf_f)) pid_data_dpds in
            if is_pid_data_deps then
              let pid_ctrl_uses = !Db.Pdg.direct_ctrl_uses pdg node in
              List.iter
                (fun n ->
                   match PdgTypes.Node.stmt n with
                     Some s ->
                     if not (List.mem s (Hashtbl.find_all pid_dependent_statements kf_f))
                     then Hashtbl.add pid_dependent_statements kf_f s
                   | _ -> Bspcheck_options.Self.error "not a statement node")
                pid_ctrl_uses;
              print_ta out s node pid_ctrl_uses
            else ();
            Cil.DoChildren
        end
      | _ -> Cil.DoChildren

  end


let get_pid_dep_statements kf_f =
  Hashtbl.find_all pid_dependent_statements kf_f


let compute () = 
  Visitor.visitFramacFileSameGlobals (new compute_pid_dep) (Ast.get())
