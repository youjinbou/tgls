module Data = Data.Tut17

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
    and fzFar  = 100000.0 in
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

  val mutable fStart = 2534.0
  val mutable fDelta = 0.0

  method private calcZOFfset () =
    let fLoopDuration = 5.0 in
    let fScale = 3.14159 *. 2.0 /. fLoopDuration in
    let fElapsedTime = (float @@ GlWindow.elapsed_time win) /. 1000.0 in
    let fCurrTimeThroughLoop = mod_float fElapsedTime fLoopDuration in
    let fRet = cos(fCurrTimeThroughLoop *. fScale) *. 500.0 -. fStart in
(*    let fRet = fDelta -. fStart in *)
    fRet

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
    let fzOffset = self#calcZOFfset () in
    Gl.uniform_3f offsetUniform 0.0 0.0 fzOffset;
    Gl.draw_elements Gl.triangles numberOfIndices Gl.unsigned_short (`Offset 0);

    Gl.bind_vertex_array 0;
    self#unuse ();
    fzOffset

  method delta dt =
    fDelta <- fDelta +. dt;
    Printf.printf "%f\n" fDelta;
    flush stdout

end

class renderer win w h = object(self)

  val prog = new shader_prog win

  val mutable width = w
  val mutable height = h

  method init_gl () = 
    prog#init ()

  method display win =
    Gl.clear_color 0.0 0.0 0.0 0.0;
    Gl.clear_depth 1.0;
    Gl.clear (Gl.bor Gl.color_buffer_bit Gl.depth_buffer_bit);
    
    let fzOffset = prog#draw () in

    check_errors ();
    GlWindow.swap_buffers win;
    self#readBackBuffer fzOffset

  val mutable bReadBuffer = false
  val mutable iFile = 0

  method readBackBuffer fzOffset : unit =
    if bReadBuffer
    then
      let pBuffer = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (500*500) in
      bReadBuffer <- false;
      Gl.read_pixels 0 0 500 500 Gl.depth_stencil Gl.unsigned_int_24_8 (`Data pBuffer);

      let error = Gl.get_error () in
      let module M = Map.Make(struct type t = int32 let compare = Pervasives.compare end) in
      let strOutput = Buffer.create (500*(500*2 + 1)) in
      let rec fillup charMap x y pBufferLoc =
	let iValue = Int32.shift_left pBuffer.{pBufferLoc} 8 in
	let iValue = Int32.logand iValue 0x00FFFFFFl in
        let charMap, c =
	  if not @@ M.mem iValue charMap
	  then if charMap <> M.empty
	       then let c = Char.chr @@ 65 + M.cardinal charMap - 1 in M.add iValue c charMap, c
	       else M.add iValue '.' charMap, '.'
          else charMap, M.find iValue charMap in
	Buffer.add_char strOutput c;
	Buffer.add_char strOutput ' ';
        let pBufferLoc = succ pBufferLoc
        and x = succ x in
        if x > 499
        then if y = 499
             then charMap  
             else begin
	         Buffer.add_char strOutput '\n';
                 fillup charMap 0 (succ y) pBufferLoc
               end
        else fillup charMap x y pBufferLoc 
      in
      let charMap = fillup M.empty 0 0 0 in
      
      let tempname = "test" ^ string_of_int iFile ^ ".txt" in
      let shaderFile = open_out tempname in
      output_string shaderFile ("Offset: " ^ string_of_float fzOffset ^ "\n");
      M.iter (fun i c -> output_string shaderFile (Int32.to_string i ^ "->\'" ^ Char.escaped c ^ "\'\n")) charMap;
      output_string shaderFile (Buffer.contents strOutput);
      Printf.printf "finished\n";
      iFile <- succ iFile

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
      | `Space  -> bReadBuffer <- true
      | `Q      -> prog#delta 100.0
      | `A      -> prog#delta (-100.0)
      (* Tens *)
      | `W      -> prog#delta 10.0
      | `S      -> prog#delta (-10.0)
      (* Ones *)
      | `E      -> prog#delta 1.0
      | `D      -> prog#delta (-1.0)
      (* Tenths *)
      | `R      -> prog#delta 0.1
      | `F      -> prog#delta (-0.1)
      (* Hundreths *)
      | `T      -> prog#delta 0.01
      | `G      -> prog#delta (-0.01)
      | _       -> ()

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
