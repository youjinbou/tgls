module Data = Data.Tut09

open TutCommon

class shader_prog (win: GlWindow.t) = object(self)

  val theProgram = Gl.create_program ()

  val vertexData = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout Data.tris

  val vertexArrayObject = TutCommon.get_int @@ Gl.gen_vertex_arrays 1
    
  val vertexBufferObject = TutCommon.get_int @@ Gl.gen_buffers 1

  val mutable offsetUniform = -1

  method colorDataIndex =
    (Gl.bigarray_byte_size vertexData) / 2 

  val xOffset = 0.5
  val yOffset = 0.5

  method setOffset () =
    Gl.uniform_2f offsetUniform xOffset yOffset

  method private initializeProgram () =
    let shader_list = [ 
      Tut.createShader Gl.fragment_shader Data.strFragmentShader;
      Tut.createShader Gl.vertex_shader Data.strVertexShader
    ] in
    ignore (Tut.createProgram theProgram shader_list);
    offsetUniform <- Gl.get_uniform_location theProgram "offset";
    let frustumScaleUnif = Gl.get_uniform_location theProgram "frustumScale"
    and zNearUnif = Gl.get_uniform_location theProgram "zNear"
    and zFarUnif = Gl.get_uniform_location theProgram "zFar" in

    self#use ();
    Gl.uniform_1f frustumScaleUnif 1.0;
    Gl.uniform_1f zNearUnif 1.0;
    Gl.uniform_1f zFarUnif  3.0;
    self#unuse ();
    List.iter Gl.delete_shader shader_list

  method private initializeVertexBuffer () =
    Gl.bind_vertex_array vertexArrayObject;

    Gl.bind_buffer Gl.array_buffer vertexBufferObject;
    Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size vertexData) (Some vertexData) Gl.static_draw;

    Gl.enable Gl.cull_face_enum;
    Gl.cull_face Gl.back;
    Gl.front_face Gl.cw
    
  method bind_vertices () =
    Gl.bind_buffer Gl.array_buffer vertexBufferObject

  method use () = 
    Gl.use_program theProgram

  method unuse () =
    Gl.use_program 0

  method init () = 
    self#initializeProgram ();
    self#initializeVertexBuffer ()

  method delete () =
    Gl.delete_program theProgram

end

class renderer win w h = object(self)

  val prog = new shader_prog win

  val mutable width = w
  val mutable height = h

  method init_gl () = 
    prog#init ()

  method display win =

    Gl.clear Gl.color_buffer_bit;

    prog#use ();
    prog#setOffset ();
    prog#bind_vertices ();

    Gl.enable_vertex_attrib_array 0;
    Gl.enable_vertex_attrib_array 1;
    Gl.vertex_attrib_pointer 0 4 Gl.float false 0 (`Offset 0);
    Gl.vertex_attrib_pointer 1 4 Gl.float false 0 (`Offset prog#colorDataIndex);
    Gl.draw_arrays Gl.triangles 0 (Data.count * 3); (* number of vertices *)

    Gl.disable_vertex_attrib_array 0;
    Gl.disable_vertex_attrib_array 1;
    prog#unuse ();

    check_errors ();
    GlWindow.swap_buffers win

  (* Handle window reshape events *)
  method reshape_cb win (w:int) (h:int) =
    width <- w;
    height <- h;
    Gl.viewport 0 0 width height

  (* Handle keyboard events *)
  method keyboard_cb win key (w:int) (h:int) =
    match key with
      | `Escape -> self#quit ()
      | _ -> ()

  method loop () = 
    GlWindow.loop win

  method quit () =
    prog#delete ();
    exit 0

  initializer (
    GlWindow.display_func win self#display;
    GlWindow.keyboard_func win self#keyboard_cb;
    GlWindow.reshape_func win self#reshape_cb;
    self#init_gl ()
  )

end

let main () =
  let width = 640
  and height = 480 in
  let w = GlWindow.create Data.title width height in
  try
    let r = new renderer w width height in
    r#loop ()
  with CompileError (src, log) ->
    prerr_endline "Compilation Error ---------------";
    prerr_endline src;
    prerr_endline "Log -----------------------------";
    prerr_endline log

let _ = main ()
