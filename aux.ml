open Cil_types

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

let print_line out () =
  Format.fprintf out "    ---------------------------------------------------------\n"

let print_in_function out (f : Cil_types.fundec) =
  Format.fprintf out "%a    In function %a\n%a\n" print_line () Printer.pp_varinfo f.svar print_line ()

let print_ints out l =
  List.iter (fun n -> Format.fprintf out "%i " n) l
