
let str = Printf.sprintf
let pp = Format.fprintf

(* String maps and sets *)

module Smap = struct

  include Map.Make(String)


  let map f s = fold (fun n e acc -> add n (f e) acc) s empty

  let of_list fname l =
    List.fold_right (fun e acc -> add (fname e) e acc) l empty

end

module Sset = struct

  include Set.Make(String)

  let map f s = fold (fun e acc -> add (f e) acc) s empty

  let of_list l =
    List.fold_right add l empty

end

let caml_to_underscores offset cname =
  let is_upper c = 'A' <= c && c <= 'Z' in
  let is_digit c = '0' <= c && c <= '9' in
  let buf = Buffer.create (String.length cname) in
(*
  let last_up = ref true (* avoids prefix by _ *) in
  for i = offset to String.length cname - 1 do
    if is_upper cname.[i] &&
       not (!last_up) &&
       not (is_digit (cname.[i - 1])) (* maps eg 2D to 2d not 2_d *)
    then (Buffer.add_char buf '_'; last_up := true)
    else (last_up := false);
    Buffer.add_char buf @@ Char.lowercase cname.[i];
  done;
 *)
  let rec accum i last_up n =
    if i < n then
      let this_up = is_upper cname.[i] || is_digit cname.[i] in
      if this_up && not last_up
      then Buffer.add_char buf '_';
      Buffer.add_char buf @@ Char.lowercase cname.[i];
      accum (succ i) this_up n
    else Buffer.contents buf
  in
  accum offset true @@ String.length cname

(* error messages *)

let info msg =
  prerr_string "info: "; Printf.fprintf stderr msg

let warning msg =
  prerr_string "warning: "; Printf.fprintf stderr msg
