
module Gl = Tgl3.Gl

let pi = acos (-1.)

exception GLError of string

let message_of_error = function
  | x when x = Gl.no_error -> "no error"
  | x when x = Gl.invalid_enum -> "invalid_enum"
  | x when x = Gl.invalid_operation -> "invalid_operation"
  | x when x = Gl.invalid_value -> "invalid_value"
  | x when x = Gl.out_of_memory -> "out_of_memory"
  | _ -> assert false
(*
  | x when x = Gl.stack_overflow -> "stack_overflow"
  | x when x = Gl.stack_underflow -> "stack_underflow"
  | x when x = Gl.table_too_large -> "table_too_large"
 *)

let check_errors () =
  match  Gl.get_error () with
      x when x = Gl.no_error -> ()
    |  e -> raise (GLError (message_of_error e))

exception CompileError of (string * string)

exception TrisOrientation of (bool * int)

let get_int =
  let a = Bigarray.(Array1.create int32 c_layout 1) in
  fun f -> f a; Int32.to_int a.{0}

let of_int =
  let a = Bigarray.(Array1.create int32 c_layout 1) in
  fun f i -> 
  a.{0} <- Int32.of_int i;
  let () = f 1 a in ()

let check_tris_orientation a n =
  let getxy a c = 
    a.(c * 4), a.(c * 4 + 1)
  in
  let signed_area a c =
      let x1, y1 = getxy a (c * 3)
      and x2, y2 = getxy a (c * 3 + 1)
      and x3, y3 = getxy a (c * 3 + 2) in
         x1 *. y2 -. x2 *. y1
      +. x2 *. y3 -. x3 *. y2
      +. x3 *. y1 -. x1 *. y3
  in
  let rec check a o c n =
    if c = n then ()
    else (
      if o = (signed_area a c > 0.0)
      then check a o (succ c) n 
      else raise (TrisOrientation (o, c)))
  in check a (signed_area a 0 > 0.0) 1 n

module Tut = struct

  let get_info_log obj =
    let open Bigarray in
    let data =
      match obj with
      | `shader shader ->
         let length = get_int @@ Gl.get_shaderiv shader Gl.info_log_length in
         let ba = Array1.create char c_layout length in
         Gl.get_shader_info_log shader length None ba;
         ba
      | `program program ->
         let length = get_int @@ Gl.get_programiv program Gl.info_log_length in
         let ba = Array1.create char c_layout length in
         Gl.get_program_info_log program length None ba;
         ba
    in
    let length = Array1.dim data in
    let nfo = Bytes.make length @@ Char.chr 0 in
    for i = 0 to pred length do
      nfo.[i] <- Array1.get data i
    done;
    nfo

  let createShader kind src =
    let shader = Gl.create_shader kind in
    Gl.shader_source shader src;
    Gl.compile_shader shader;
    if get_int @@ Gl.get_shaderiv shader Gl.compile_status = 1
    then shader
    else raise (CompileError (src, get_info_log @@ `shader shader))
      
  let createProgram program shader_list = 
    List.iter (fun shader -> Gl.attach_shader program shader) shader_list;
    Gl.link_program program;
    if 1 <> get_int @@ Gl.get_programiv program Gl.link_status
    then raise (CompileError ("link", get_info_log @@ `program program));
    List.iter (fun shader -> Gl.detach_shader program shader) shader_list

end

(*
module GlutWindow = struct

  type t = unit

  let create title : t =
    ignore (Glut.init Sys.argv);
    Glut.initDisplayMode ~alpha:true ~depth:true ~double_buffer:true ();
    Glut.initWindowSize width height;
    ignore (Glut.createWindow title);
    

  let swap_buffers (v : t) =
    Glut.swapBuffers ();
    Glut.postRedisplay ()

  let loop (v : t) =
    Glut.mainloop ()

  let display_func (w: t) f =
    Glut.displayFunc f

  let keyboard_func (w: t) f =
    Glut.keyboardFunc f

  let reshape_func (w: t) f =
    Glut.reshapeFunc f
 
  let elapsed_time (w: t) =
    Glut.(get ELAPSED_TIME)

end
 *)

module GlWindow = struct

  open Tsdl

  type key = 
    [ `A | `Ac_back | `Ac_bookmarks | `Ac_forward | `Ac_home | `Ac_refresh | `Ac_search | `Ac_stop
    | `Again | `Alterase | `Apostrophe | `App1 | `App2 | `Application | `Audiomute | `Audionext
    | `Audioplay | `Audioprev | `Audiostop | `B | `Backslash | `Backspace | `Brightnessdown
    | `Brightnessup | `C | `Calculator | `Cancel | `Capslock | `Clear | `Clearagain | `Comma
    | `Computer | `Copy | `Crsel | `Currencysubunit | `Currencyunit | `Cut | `D | `Decimalseparator
    | `Delete | `Displayswitch | `Down | `E | `Eject | `End | `Equals | `Escape | `Execute | `Exsel
    | `F | `F1 | `F10 | `F11 | `F12 | `F13 | `F14 | `F15 | `F16 | `F17 | `F18 | `F19 | `F2 | `F20
    | `F21 | `F22 | `F23 | `F24 | `F3 | `F4 | `F5 | `F6 | `F7 | `F8 | `F9 | `Find | `G | `Grave
    | `H | `Help | `Home | `I | `Insert | `International1 | `International2 | `International3
    | `International4 | `International5 | `International6 | `International7 | `International8
    | `International9 | `J | `K | `K0 | `K1 | `K2 | `K3 | `K4 | `K5 | `K6 | `K7 | `K8 | `K9
    | `Kbdillumdown | `Kbdillumtoggle | `Kbdillumup | `Kp_0 | `Kp_00 | `Kp_000 | `Kp_1 | `Kp_2
    | `Kp_3 | `Kp_4 | `Kp_5 | `Kp_6 | `Kp_7 | `Kp_8 | `Kp_9 | `Kp_a | `Kp_ampersand | `Kp_at
    | `Kp_b | `Kp_backspace | `Kp_binary | `Kp_c | `Kp_clear | `Kp_clearentry | `Kp_colon
    | `Kp_comma | `Kp_d | `Kp_dblampersand | `Kp_dblverticalbar | `Kp_decimal | `Kp_divide
    | `Kp_e | `Kp_enter | `Kp_equals | `Kp_equalsas400 | `Kp_exclam | `Kp_f | `Kp_greater
    | `Kp_hash | `Kp_hexadecimal | `Kp_leftbrace | `Kp_leftparen | `Kp_less | `Kp_memadd
    | `Kp_memclear | `Kp_memdivide | `Kp_memmultiply | `Kp_memrecall | `Kp_memstore
    | `Kp_memsubtract | `Kp_minus | `Kp_multiply | `Kp_octal | `Kp_percent | `Kp_period
    | `Kp_plus | `Kp_plusminus | `Kp_power | `Kp_rightbrace | `Kp_rightparen | `Kp_space
    | `Kp_tab | `Kp_verticalbar | `Kp_xor | `L | `Lalt | `Lang1 | `Lang2 | `Lang3 | `Lang4
    | `Lang5 | `Lang6 | `Lang7 | `Lang8 | `Lang9 | `Lctrl | `Left | `Leftbracket | `Lgui
    | `Lshift | `M | `Mail | `Mediaselect | `Menu | `Minus | `Mode | `Mute | `N
    | `Nonusbackslash | `Nonushash | `Numlockclear | `O | `Oper | `Out | `P
    | `Pagedown | `Pageup | `Paste | `Pause | `Period | `Power | `Printscreen | `Prior
    | `Q | `R | `Ralt | `Rctrl | `Return | `Return2 | `Rgui | `Right | `Rightbracket
    | `Rshift | `S | `Scrolllock | `Select | `Semicolon | `Separator | `Slash | `Sleep
    | `Space | `Stop | `Sysreq | `T | `Tab | `Thousandsseparator | `U | `Undo | `Unknown
    | `Up | `V | `Volumedown | `Volumeup | `W | `Www | `X | `Y | `Z ]

  type 'a display_func = 'a -> unit
  type 'a keyboard_func = 'a -> key -> int -> int -> unit
  type 'a reshape_func = 'a -> int -> int -> unit

  type t = {
    win : Sdl.window;
    ctx : Sdl.gl_context;
    start : float;
    mutable display : t display_func;
    mutable keyboard: t keyboard_func;
    mutable reshape : t reshape_func;
  }

  let dummy_display : t display_func = fun _ -> ()
  let dummy_keybard : t keyboard_func = fun  _ _ _ _ -> ()
  let dummy_reshape : t reshape_func = fun _ _ _ -> ()

  let mk win ctx =
    {win;ctx; start = Unix.gettimeofday (); display = dummy_display; keyboard = dummy_keybard; reshape = dummy_reshape} 
  
  let create title w h : t =
    let w_atts = Sdl.Window.(opengl + resizable) in
    let set a v = ignore (Sdl.gl_set_attribute a v) in
    Sdl.Gl.(
      set context_profile_mask context_profile_core;
      set context_major_version 3;
      set context_minor_version 3;
      set doublebuffer 1
    );
    match Sdl.create_window ~w ~h title w_atts with
      `Ok win -> (
      match Sdl.gl_create_context win with
        `Ok ctx -> ignore (Sdl.gl_make_current win ctx);
                   (* Sdl.log "%a" pp_opengl_info (); *)
                   mk win ctx
      | _ -> failwith "unable to create gl context"
    )
    | _ -> failwith "unable to create window"

  let swap_buffers (t : t) =
    Sdl.gl_swap_window t.win

  let display_func (t: t) f =
    t.display <- f

  let keyboard_func (t: t) f =
    t.keyboard <- f

  let reshape_func (t: t) f =
    t.reshape <- f

  let loop (t : t) =
    let e = Sdl.Event.create () in
    let key_scancode e = Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode) in
    let window_event e = Sdl.Event.(window_event_enum (get e window_event_id)) in
    let rec handle_events () =
      let () = t.display t in
      match Sdl.poll_event (Some e) with
      | true -> (
        match  Sdl.Event.(enum @@ get e typ) with
        | `Quit -> ()
        | `Key_down | `Key_up -> t.keyboard t (key_scancode e) 0 0; handle_events ()
        | `Window_event -> (
          match window_event e with
          | `Exposed | `Resized -> let w, h = Sdl.get_window_size t.win in
                                   t.reshape t w h; handle_events ()
          | _ -> handle_events ()
        )
        | _ -> handle_events ()
      )
      | _ -> loop ()
      and loop () =
      let () = t.display t in
      handle_events ()

    in loop ()

  let elapsed_time (t: t) : int =
    let ctime = Unix.gettimeofday () in
    int_of_float (1000.0 *. (ctime -. t.start))

end

(*
module BigarrayHelper = struct

  open Bigarray
  (* I want a way of getting the size of an element from a bigarray *)
 
  type _ sizeof = 
    | Complex32 : complex32_elt sizeof
    | Complex64 : complex64_elt sizeof
    | Float32: float32_elt sizeof
    | Float64: float64_elt sizeof
    | Int16_Signed_elt  : int16_signed_elt sizeof
    | Int16_Unsigned_elt  : int16_unsigned_elt sizeof
    | Int32 : int32_elt sizeof
    | Int64 : int64_elt sizeof
    | Int8_Signed_elt : int8_signed_elt sizeof
    | Int8_Unsigned_elt : int8_unsigned_elt sizeof
    | Int : int_elt sizeof
    | NativeInt : nativeint_elt sizeof

  let element_size : type a.  (_,a,_) Array1.t -> int = fun b ->
 *)
