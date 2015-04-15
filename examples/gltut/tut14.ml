module Data = Data.Tut14

open TutCommon

class shader_prog (win: GlWindow.t) = object(self)

  val theProgram = Gl.create_program ()

  val vertexData = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout Data.tris

  val indexData = Bigarray.Array1.of_array Bigarray.int16_unsigned Bigarray.c_layout Data.indices

  val vao = TutCommon.get_int @@ Gl.gen_vertex_arrays 1

  val vertexBufferObject = TutCommon.get_int @@ Gl.gen_buffers 1
  val indexBufferObject = TutCommon.get_int @@ Gl.gen_buffers 1

  val mutable offsetUniform = -1
  val mutable perspectiveMatrixUnif = -1

  val perspectiveMatrix = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 16

  val fFrustumScale = 1.0

  val numberOfVertices = Data.count
  val numberOfIndices = Array.length Data.indices

  method private initializeProgram () =
    let shader_list = [
      Tut.createShader Gl.fragment_shader Data.strFragmentShader;
      Tut.createShader Gl.vertex_shader Data.strVertexShader
    ] in
    ignore (Tut.createProgram theProgram shader_list);
    offsetUniform <- Gl.get_uniform_location theProgram "offset";
    perspectiveMatrixUnif <- Gl.get_uniform_location theProgram "perspectiveMatrix";
    
    let fzNear = 1.0
    and fzFar  = 3.0 in
    let a0  = fFrustumScale
    and a5  = fFrustumScale
    and a10 = (fzFar +. fzNear) /. (fzNear -. fzFar)
    and a14 = (2. *. fzFar *. fzNear) /. (fzNear -. fzFar)
    and a11 = -1.0 in
    for i = 0 to 15 do perspectiveMatrix.{i} <- 0.0 done;
    perspectiveMatrix.{0} <- a0;
    perspectiveMatrix.{5} <- a5;
    perspectiveMatrix.{10} <- a10;
    perspectiveMatrix.{11} <- a11;
    perspectiveMatrix.{14} <- a14;
    self#use ();
    Gl.uniform_matrix_4fv perspectiveMatrixUnif 1 false perspectiveMatrix;
    self#unuse ();
    List.iter Gl.delete_shader shader_list

  method reshape w h = 
    perspectiveMatrix.{0} <- fFrustumScale /. ((float w) /. (float h));
    perspectiveMatrix.{5} <- fFrustumScale;
    self#use ();
    Gl.uniform_matrix_4fv perspectiveMatrixUnif 1 false perspectiveMatrix;
    self#unuse ()

  method private initializeVertexBuffer () = 

    Gl.bind_buffer Gl.array_buffer vertexBufferObject;
    Gl.buffer_data Gl.array_buffer (Gl.bigarray_byte_size vertexData) (Some vertexData) Gl.static_draw;
    Gl.bind_buffer Gl.array_buffer 0;

    Gl.bind_buffer Gl.element_array_buffer indexBufferObject;
    Gl.buffer_data Gl.element_array_buffer (Gl.bigarray_byte_size indexData) (Some indexData) Gl.static_draw;
    Gl.bind_buffer Gl.element_array_buffer 0

   method private initializeVertexArrayObjects () =
    Gl.bind_vertex_array vao;

    let float_size = 4 (* bytes *) in

    let colorDataOffset = 
      float_size * 3 * numberOfVertices in

    Gl.bind_buffer Gl.array_buffer vertexBufferObject;
    Gl.enable_vertex_attrib_array 0;
    Gl.enable_vertex_attrib_array 1;
    Gl.vertex_attrib_pointer 0 3 Gl.float false 0 (`Offset 0);
    Gl.vertex_attrib_pointer 1 4 Gl.float false 0 (`Offset colorDataOffset);
    Gl.bind_buffer Gl.element_array_buffer indexBufferObject;

    Gl.bind_vertex_array 0;
    Gl.bind_buffer Gl.array_buffer 0;
    Gl.bind_buffer Gl.element_array_buffer 0

  method bind_vertices () =
    Gl.bind_buffer Gl.array_buffer vertexBufferObject

  method use () = 
    Gl.use_program theProgram

  method unuse () =
    Gl.use_program 0

  method init () = 
    self#initializeProgram ();
    self#initializeVertexBuffer ();
    self#initializeVertexArrayObjects ();

    Gl.enable Gl.cull_face_enum;
    Gl.cull_face Gl.back;
    Gl.front_face Gl.cw;
    Gl.enable Gl.depth_test;
    Gl.depth_mask true;
    Gl.depth_func Gl.lequal;
    Gl.depth_range 0.0 1.0

  method delete () =
    Gl.delete_program theProgram

  method draw () =
    self#use ();
    Gl.bind_vertex_array vao;
    
    Gl.uniform_3f offsetUniform 0.0 0.0 (-1.0);
    Gl.draw_elements Gl.triangles numberOfIndices Gl.unsigned_short (`Offset 0);

    Gl.uniform_3f offsetUniform 0.0 0.0 (-1.0);
    (* this function seems broken with nvidia driver 295 *)
    Gl.draw_elements_base_vertex Gl.triangles numberOfIndices Gl.unsigned_short (`Offset 0) (numberOfVertices / 2);
    
    Gl.bind_vertex_array 0;
    self#unuse ()

end

class renderer win w h = object(self)

  val prog = new shader_prog win

  val mutable width = w
  val mutable height = h

  method init_gl () = 
    prog#init ()

  method display win =

    Gl.clear (Gl.bor Gl.color_buffer_bit Gl.depth_buffer_bit);
    
    prog#draw ();

    check_errors ();
    GlWindow.swap_buffers win

  (* Handle window reshape events *)
  method reshape_cb win (w:int) (h:int) =
    width <- w;
    height <- h;
    prog#reshape w h;
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
