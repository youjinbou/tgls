open Utils
open Glreg

(* TODO: replace enum list with the type below reflecting the real type of enum groups *)
type rgroup_def =
[ `Enum of string list       (* really an enum *)
| `Bitfield of string list   (* a bitfield *)
| `Alias of string  ]        (* actually just a typedef *)

(* augmented registry group  *)
type 'a group = {
  g_name        : string;
  mutable g_def : 'a;
  g_decl_kind   : [`Implicit | `Explicit | `Combined];
  (* whether the group was declared:
   * - explicitely (in a <group> tag), 
   * - implicitely (in an enum or command definition),
   * - combined from an explicit and implicit definition *)
}

let mk_group g_name g_def g_decl_kind =
  { g_name; g_def; g_decl_kind }

(* build groups out of enums and groups *)
let build_groups r =
  (* We don't need redundant enums, ie. enums with different names, but
   * same value and same _group_, thus we remove enums with same value of groups.
   * Usually, redundant enums come from vendor extensions which have been normalized. 
   * Ideally, we'd want to gather all the enums, and remove all those which belong to
   * extensions to obtain profile only enums.
   * => 
   * - get all the required enums from the feature set (done in Capi)
   * - get all the required groups out of the required functions, and update their type
   *   (enum set, bitfield set or plain type)
   * -> remove all unecessary enums from groups by only keeping those selected earlier
 *)
  let reduce_group (enums : string list) =
(*
    let vmap_add vmap name =
      try
        let e_def = 
          match Hashtbl.find r.enums name with
          | [x] -> x
          | [] -> raise Not_found
          | x::xs -> failwith (str "multiple definitions for enum %s" name)
        in 
        try
          let e = Smap.find e_def.e_value vmap in
          let e = if String.length e.e_name <= String.length e_def.e_name then e else e_def in
          let vmap = Smap.remove e.e_value vmap in
          Smap.add e.e_value e vmap 
        with Not_found -> Smap.add e_def.e_value e_def vmap
      with Not_found -> warning "no definition for enum %s\n" name; vmap
    in 
    let vmap = List.fold_left vmap_add Smap.empty enums in
    List.rev @@ Smap.fold (fun _ v l -> v.e_name::l) vmap []
 *)
    let filter name =
      Hashtbl.mem r.enums name
    in
    List.filter filter enums
  in
  (* add enum definition [e_def] to new groups hashtbl (ngroups), creating the group if necessary *)
  let build_implicit_groups enums =
    (* group new definitions [ngroups] *)
    let ngroups :  (string, string list group) Hashtbl.t = Hashtbl.create 503 in
    let add_enum e_def =
      match e_def.e_p_group with
        Some g_name -> (
        try
          (* check if the group already exists *)
          let g = Hashtbl.find ngroups g_name in
          (* add the enum to the group *)
          g.g_def <- e_def.e_name :: g.g_def
        with Not_found ->
          (* create the group *)
          Hashtbl.add ngroups g_name { g_name; g_def = [e_def.e_name]; 
                                        g_decl_kind = `Implicit }
      )
      | None -> (* warning "enum '%s' with no group\n" e_def.e_name; (*  ~5000 messages! *) *) ()
    in
    (* add all the enums in the new group definitions [ngroups] *)
    Hashtbl.iter (fun _ l -> List.iter add_enum l) enums;
    ngroups
  in
  let ngroups = build_implicit_groups r.enums in
  (* merge sets of groups *)
  let merge_list l1 l2 =
    let set = List.fold_left (fun set e -> Sset.add e set) Sset.empty l1 in
    let set = List.fold_left (fun set e -> Sset.add e set) set l2 in
    Sset.elements set
  in
  info "groups : explicit %d, implicit %d\n" (Hashtbl.length r.groups) (Hashtbl.length ngroups);
  (* include all the explicit groups to the implicit (generated) groups *)
  let fold (* ng *) name g =
    try
      let ng = Hashtbl.find ngroups name in
      Hashtbl.replace ngroups name @@
        mk_group ng.g_name (merge_list g.g_enums ng.g_def) `Combined
    with Not_found ->
      Hashtbl.add ngroups name @@ mk_group g.g_name g.g_enums `Explicit
  in
  Hashtbl.iter fold r.groups;
  (* reduce each group to its core set of enums *)
  Hashtbl.iter (fun _ g -> g.g_def <- reduce_group g.g_def) ngroups;
  info "groups : total %d\n" (Hashtbl.length ngroups);
  ngroups


type 'a t =
  { types : (string, typ list) Hashtbl.t;
    groups : (string, 'a group) Hashtbl.t;
    enums : (string, enum list) Hashtbl.t;
    commands : (string, command list) Hashtbl.t;
    features : (string, feature list) Hashtbl.t;
    extensions : (string, extension) Hashtbl.t; }
