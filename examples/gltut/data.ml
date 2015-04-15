open TutCommon

module Tut01 = struct

  let title = "Tut 01 Hello Triangle"

  let strVertexShader = "
      #version 330
      layout (location = 0) in vec4 position;
      void main()
      {
         gl_Position = position;
      }"

  let strFragmentShader = "
      #version 330
      out vec4 outputColor;
      void main()
      {
         outputColor = vec4(1.0f, 1.0f, 1.0f, 1.0f);
      }"

  let count = 3 (* number of vertices *)

  let tris = 
    [|
      0.75;  0.75; 0.0; 1.0;
      0.75; -0.75; 0.0; 1.0;
     -0.75; -0.75; 0.0; 1.0;
    |]
end

module Tut02 = struct

  let title = "Tut 02 Fragment Position"

  let strVertexShader = "
      #version 330

      layout (location = 0) in vec4 position;

      void main()
      {
         gl_Position = position;
      }"

  let strFragmentShader = "
      #version 330

      out vec4 outputColor;

      void main()
      {
          float lerpValue = gl_FragCoord.y / 500.0f;
      
          outputColor = mix(vec4(1.0f, 1.0f, 1.0f, 1.0f),
          vec4(0.2f, 0.2f, 0.2f, 1.0f), lerpValue);
      }" 


  let count = Tut01.tris
  let tris  = Tut01.tris

end

module Tut03 = struct

  let title = "Tut 02 Vertex Colors"

  let strVertexShader = "
      #version 330

      layout (location = 0) in vec4 position;
      layout (location = 1) in vec4 color;

      smooth out vec4 theColor;

      void main()
      {
         gl_Position = position;
         theColor = color;
      }"

  let strFragmentShader = "
      #version 330

      smooth in vec4 theColor;

      out vec4 outputColor;

      void main()
      {
         outputColor = theColor;
      }" 

  let count = 3
  let tris =
    [|
     0.0;    0.5; 0.0; 1.0;
     0.5; -0.366; 0.0; 1.0;
    -0.5; -0.366; 0.0; 1.0;
     1.0;    0.0; 0.0; 1.0;
     0.0;    1.0; 0.0; 1.0;
     0.0;    0.0; 1.0; 1.0
    |]
end

module Tut04 = struct

  let title = "Tut 03 CPU Position Offset"

  let strVertexShader = Tut01.strVertexShader

  let strFragmentShader = Tut01.strFragmentShader

  let tris = 
    [|
 	0.25;  0.25; 0.0; 1.0;
	0.25; -0.25; 0.0; 1.0;
       -0.25; -0.25; 0.0; 1.0;
    |]

  let count = 3

end

module Tut05 = struct

  let title = "Tut 03 Shader Position Offset"

  let strVertexShader = "
      #version 330

      layout(location = 0) in vec4 position;
      uniform vec2 offset;

      void main()
      {
          vec4 totalOffset = vec4(offset.x, offset.y, 0.0, 0.0);
          gl_Position = position + totalOffset;
      }"

  let strFragmentShader = "
      #version 330

      out vec4 outputColor;

      void main()
      {
          outputColor = vec4(1.0f, 1.0f, 1.0f, 1.0f);
      }" 

  let count = Tut04.count
  let tris = Tut04.tris

end

module Tut06 = struct

  let title = "Tut 03 Shader Calc Offset"

  let strVertexShader = "
      #version 330

      layout(location = 0) in vec4 position;
      uniform float loopDuration;
      uniform float time;

      void main()
      {
          float timeScale = 3.14159f * 2.0f / loopDuration;
      
          float currTime = mod(time, loopDuration);
          vec4 totalOffset = vec4(
              cos(currTime * timeScale) * 0.5f,
              sin(currTime * timeScale) * 0.5f,
              0.0f,
              0.0f);

          gl_Position = position + totalOffset;
      }"

  let strFragmentShader = Tut05.strFragmentShader

  let count = Tut04.count
  let tris = Tut04.tris

end

module Tut07 = struct

  let title = "Tut 03 Fragment Change Color"

  let strVertexShader = Tut06.strVertexShader

  let strFragmentShader = "
      #version 330

      out vec4 outputColor;

      uniform float fragLoopDuration;
      uniform float time;

      const vec4 firstColor = vec4(1.0f, 1.0f, 1.0f, 1.0f);
      const vec4 secondColor = vec4(0.0f, 1.0f, 0.0f, 1.0f);


      void main()
      {
          float currTime = mod(time, fragLoopDuration);
          float currLerp = currTime / fragLoopDuration;

          outputColor = mix(firstColor, secondColor, currLerp);
      }" 

  let count = Tut04.tris

  let tris  = Tut04.tris

end

module Tut08 = struct

  let title = "Tut 04 Orthographic Cube"

  let strVertexShader = "
      #version 330

      layout(location = 0) in vec4 position;
      layout(location = 1) in vec4 color;
      
      smooth out vec4 theColor;

      uniform vec2 offset;


      void main()
      {

         gl_Position = position + vec4(offset.x, offset.y, 0.0, 0.0);
         theColor = color;

      }"

  let strFragmentShader = "
      #version 330

      smooth in vec4 theColor;

      out vec4 outputColor;

      void main()
      {
          outputColor = theColor;
      }" 

  let tris = [|

	 0.25;  0.25; 0.75; 1.0; (* 0 *)
	 0.25; -0.25; 0.75; 1.0; (* 1 *)
	-0.25;  0.25; 0.75; 1.0; (* 2 *)

	 0.25; -0.25; 0.75; 1.0; (* 3 *)
	-0.25; -0.25; 0.75; 1.0; (* 4 *)
	-0.25;  0.25; 0.75; 1.0; (* 5 *)

	 0.25;  0.25; -0.75; 1.0; (* 6 *)
	-0.25;  0.25; -0.75; 1.0; (* 7 *)
	 0.25; -0.25; -0.75; 1.0; (* 8 *)

	 0.25; -0.25; -0.75; 1.0; (* 9 *)
	-0.25;  0.25; -0.75; 1.0; (* 10 *)
	-0.25; -0.25; -0.75; 1.0; (* 11 *)

	-0.25;  0.25;  0.75; 1.0; (* 12 *)
	-0.25; -0.25;  0.75; 1.0; (* 13 *)
	-0.25; -0.25; -0.75; 1.0; (* 14 *)

	-0.25;  0.25;  0.75; 1.0; (* 15 *)
	-0.25; -0.25; -0.75; 1.0; (* 16 *)
	-0.25;  0.25; -0.75; 1.0; (* 17 *)

	 0.25;  0.25;  0.75; 1.0; (* 18 *)
	 0.25; -0.25; -0.75; 1.0; (* 19 *)
	 0.25; -0.25;  0.75; 1.0; (* 20 *)

	 0.25;  0.25;  0.75; 1.0; (* 21 *)
	 0.25;  0.25; -0.75; 1.0; (* 22 *)
	 0.25; -0.25; -0.75; 1.0; (* 23 *)

	 0.25;  0.25; -0.75; 1.0; (* 24 *)
	 0.25;  0.25;  0.75; 1.0; (* 25 *)
	-0.25;  0.25;  0.75; 1.0; (* 26 *)

	 0.25;  0.25; -0.75; 1.0; (* 27 *)
	-0.25;  0.25;  0.75; 1.0; (* 28 *)
	-0.25;  0.25; -0.75; 1.0; (* 29 *)

	 0.25; -0.25; -0.75; 1.0; (* 30 *)
	-0.25; -0.25;  0.75; 1.0; (* 31 *)
	 0.25; -0.25;  0.75; 1.0; (* 32 *)

	 0.25; -0.25; -0.75; 1.0; (* 33 *)
	-0.25; -0.25; -0.75; 1.0; (* 34 *)
	-0.25; -0.25;  0.75; 1.0; (* 35 *)



        0.0; 0.0; 1.0; 1.0; (* 0 *)
        0.0; 0.0; 1.0; 1.0; (* 1 *)
        0.0; 0.0; 1.0; 1.0; (* 2 *)

        0.0; 0.0; 1.0; 1.0; (* 3 *)
        0.0; 0.0; 1.0; 1.0; (* 4 *)
        0.0; 0.0; 1.0; 1.0; (* 5 *)

        0.8; 0.8; 0.8; 1.0; (* 6 *)
        0.8; 0.8; 0.8; 1.0; (* 7 *)
        0.8; 0.8; 0.8; 1.0; (* 8 *)

        0.8; 0.8; 0.8; 1.0; (* 9 *)
        0.8; 0.8; 0.8; 1.0; (* 10 *)
        0.8; 0.8; 0.8; 1.0; (* 11 *)

        0.0; 1.0; 0.0; 1.0; (* 12 *)
        0.0; 1.0; 0.0; 1.0; (* 13 *)
        0.0; 1.0; 0.0; 1.0; (* 14 *)

        0.0; 1.0; 0.0; 1.0; (* 15 *)
        0.0; 1.0; 0.0; 1.0; (* 16 *)
        0.0; 1.0; 0.0; 1.0; (* 17 *)

        0.5; 0.5; 0.0; 1.0; (* 18 *)
        0.5; 0.5; 0.0; 1.0; (* 19 *)
        0.5; 0.5; 0.0; 1.0; (* 20 *)

        0.5; 0.5; 0.0; 1.0; (* 21 *)
        0.5; 0.5; 0.0; 1.0; (* 22 *)
        0.5; 0.5; 0.0; 1.0; (* 23 *)

        1.0; 0.0; 0.0; 1.0; (* 24 *)
        1.0; 0.0; 0.0; 1.0; (* 25 *)
        1.0; 0.0; 0.0; 1.0; (* 26 *)

        1.0; 0.0; 0.0; 1.0; (* 27 *)
        1.0; 0.0; 0.0; 1.0; (* 28 *)
        1.0; 0.0; 0.0; 1.0; (* 29 *)

        0.0; 1.0; 1.0; 1.0; (* 30 *)
        0.0; 1.0; 1.0; 1.0; (* 31 *)
        0.0; 1.0; 1.0; 1.0; (* 32 *)

        0.0; 1.0; 1.0; 1.0; (* 33 *)
        0.0; 1.0; 1.0; 1.0; (* 34 *)
        0.0; 1.0; 1.0; 1.0; (* 35 *)

    |]

  let count = (Array.length tris) / 24 (* tris count *)


end

module Tut09 = struct

  let title = "Tut 04 ShaderPerspective"

  let strVertexShader = "
      #version 330

      layout(location = 0) in vec4 position;
      layout(location = 1) in vec4 color;

      smooth out vec4 theColor;

      uniform vec2 offset;
      uniform float zNear;
      uniform float zFar;
      uniform float frustumScale;

      void main()
      {
          vec4 cameraPos = position + vec4(offset.x, offset.y, 0.0, 0.0);
          vec4 clipPos;
      
          clipPos.xy = cameraPos.xy * frustumScale;
      
          clipPos.z = cameraPos.z * (zNear + zFar) / (zNear - zFar);
          clipPos.z += 2 * zNear * zFar / (zNear - zFar);
      
          clipPos.w = -cameraPos.z;
      
          gl_Position = clipPos;
          theColor = color;
      }"

  let strFragmentShader = "
      #version 330

      smooth in vec4 theColor;

      out vec4 outputColor;

      void main()
      {
          outputColor = theColor;
      }" 

  let tris = [|
	 0.25;  0.25; -1.25; 1.0;
	 0.25; -0.25; -1.25; 1.0;
	-0.25;  0.25; -1.25; 1.0;

	 0.25; -0.25; -1.25; 1.0;
	-0.25; -0.25; -1.25; 1.0;
	-0.25;  0.25; -1.25; 1.0;

	 0.25;  0.25; -2.75; 1.0;
	-0.25;  0.25; -2.75; 1.0;
	 0.25; -0.25; -2.75; 1.0;

	 0.25; -0.25; -2.75; 1.0;
	-0.25;  0.25; -2.75; 1.0;
	-0.25; -0.25; -2.75; 1.0;

	-0.25;  0.25; -1.25; 1.0;
	-0.25; -0.25; -1.25; 1.0;
	-0.25; -0.25; -2.75; 1.0;

	-0.25;  0.25; -1.25; 1.0;
	-0.25; -0.25; -2.75; 1.0;
	-0.25;  0.25; -2.75; 1.0;

	 0.25;  0.25; -1.25; 1.0;
	 0.25; -0.25; -2.75; 1.0;
	 0.25; -0.25; -1.25; 1.0;

	 0.25;  0.25; -1.25; 1.0;
	 0.25;  0.25; -2.75; 1.0;
	 0.25; -0.25; -2.75; 1.0;

	 0.25;  0.25; -2.75; 1.0;
	 0.25;  0.25; -1.25; 1.0;
	-0.25;  0.25; -1.25; 1.0;

	 0.25;  0.25; -2.75; 1.0;
	-0.25;  0.25; -1.25; 1.0;
	-0.25;  0.25; -2.75; 1.0;

	 0.25; -0.25; -2.75; 1.0;
	-0.25; -0.25; -1.25; 1.0;
	 0.25; -0.25; -1.25; 1.0;

	 0.25; -0.25; -2.75; 1.0;
	-0.25; -0.25; -2.75; 1.0;
	-0.25; -0.25; -1.25; 1.0;


	0.0; 0.0; 1.0; 1.0;
	0.0; 0.0; 1.0; 1.0;
	0.0; 0.0; 1.0; 1.0;

	0.0; 0.0; 1.0; 1.0;
	0.0; 0.0; 1.0; 1.0;
	0.0; 0.0; 1.0; 1.0;

	0.8; 0.8; 0.8; 1.0;
	0.8; 0.8; 0.8; 1.0;
	0.8; 0.8; 0.8; 1.0;

	0.8; 0.8; 0.8; 1.0;
	0.8; 0.8; 0.8; 1.0;
	0.8; 0.8; 0.8; 1.0;

	0.0; 1.0; 0.0; 1.0;
	0.0; 1.0; 0.0; 1.0;
	0.0; 1.0; 0.0; 1.0;

	0.0; 1.0; 0.0; 1.0;
	0.0; 1.0; 0.0; 1.0;
	0.0; 1.0; 0.0; 1.0;

	0.5; 0.5; 0.0; 1.0;
	0.5; 0.5; 0.0; 1.0;
	0.5; 0.5; 0.0; 1.0;

	0.5; 0.5; 0.0; 1.0;
	0.5; 0.5; 0.0; 1.0;
	0.5; 0.5; 0.0; 1.0;

	1.0; 0.0; 0.0; 1.0;
	1.0; 0.0; 0.0; 1.0;
	1.0; 0.0; 0.0; 1.0;

	1.0; 0.0; 0.0; 1.0;
	1.0; 0.0; 0.0; 1.0;
	1.0; 0.0; 0.0; 1.0;

	0.0; 1.0; 1.0; 1.0;
	0.0; 1.0; 1.0; 1.0;
	0.0; 1.0; 1.0; 1.0;

	0.0; 1.0; 1.0; 1.0;
	0.0; 1.0; 1.0; 1.0;
	0.0; 1.0; 1.0; 1.0;
  |]

  let count = (Array.length tris) / 24 (* tris count *)

end


module Tut10 = struct

  let title = "Tut 04 MatrixPerspective"

  let strVertexShader = "
      #version 330

      layout(location = 0) in vec4 position;
      layout(location = 1) in vec4 color;
      
      smooth out vec4 theColor;

      uniform vec2 offset;
      uniform mat4 perspectiveMatrix;

      void main()
      {
         vec4 cameraPos = position + vec4(offset.x, offset.y, 0.0, 0.0);

         gl_Position = perspectiveMatrix * cameraPos;
         theColor = color;
      }"


  let strFragmentShader = "
      #version 330

      smooth in vec4 theColor;

      out vec4 outputColor;

      void main()
      {
          outputColor = theColor;
      }" 

    
  let tris = Tut09.tris

  let count = Tut09.count

end

module Tut11 = struct

  let title = "Tut 04 Aspect Ratio"

  let strVertexShader = Tut10.strVertexShader

  let strFragmentShader = Tut10.strFragmentShader

  let tris = Tut09.tris

  let count = Tut09.count


end

module Tut12 = struct

  let title = "Tut 05 Overlap No Depth"

  let strVertexShader = "
    #version 330

    layout(location = 0) in vec4 position;
    layout(location = 1) in vec4 color;

    smooth out vec4 theColor;

    uniform vec3 offset;
    uniform mat4 perspectiveMatrix;

    void main()
    {
        vec4 cameraPos = position + vec4(offset.x, offset.y, offset.z, 0.0);

        gl_Position = perspectiveMatrix * cameraPos;
        theColor = color;
    }"


  let strFragmentShader = "
    #version 330

    smooth in vec4 theColor;

    out vec4 outputColor;

    void main()
    {
        outputColor = theColor;
    }"

  let right_extent  =  0.8
  let left_extent   = -.right_extent
  let top_extent    =  0.20
  let middle_extent =  0.0
  let bottom_extent = -.top_extent
  let front_extent  = -1.25
  let rear_extent   = -1.75

  let green = [| 0.75; 0.75; 1.0; 1.0 |]
  let blue  = [| 0.0; 0.5; 0.0; 1.0 |]
  let red   = [| 1.0; 0.0; 0.0; 1.0 |]
  let grey  = [| 0.8; 0.8; 0.8; 1.0 |]
  let brown = [| 0.5; 0.5; 0.0; 1.0 |]

  let count = 36 (* total number of vertices *)

  let tris =
    [|
      (* Object 1 positions *)
      left_extent;	top_extent;	rear_extent;
      left_extent;	middle_extent;	front_extent;
      right_extent;	middle_extent;	front_extent;
      right_extent;	top_extent;	rear_extent;

      left_extent;	bottom_extent;	rear_extent;
      left_extent;	middle_extent;	front_extent;
      right_extent;	middle_extent;	front_extent;
      right_extent;	bottom_extent;	rear_extent;

      left_extent;	top_extent;	rear_extent;
      left_extent;	middle_extent;	front_extent;
      left_extent;	bottom_extent;	rear_extent;

      right_extent;	top_extent;	rear_extent;
      right_extent;	middle_extent;	front_extent;
      right_extent;	bottom_extent;	rear_extent;

      left_extent;	bottom_extent;	rear_extent;
      left_extent;	top_extent;	rear_extent;
      right_extent;	top_extent;	rear_extent;
      right_extent;	bottom_extent;	rear_extent;

      (* Object 2 positions *)
      top_extent;	right_extent;	rear_extent;
      middle_extent;	right_extent;	front_extent;
      middle_extent;	left_extent;	front_extent;
      top_extent;	left_extent;	rear_extent;

      bottom_extent;	right_extent;	rear_extent;
      middle_extent;	right_extent;	front_extent;
      middle_extent;	left_extent;	front_extent;
      bottom_extent;	left_extent;	rear_extent;

      top_extent;	right_extent;	rear_extent;
      middle_extent;	right_extent;	front_extent;
      bottom_extent;	right_extent;	rear_extent;
      
      top_extent;	left_extent;	rear_extent;
      middle_extent;	left_extent;	front_extent;
      bottom_extent;	left_extent;	rear_extent;
      
      bottom_extent;	right_extent;	rear_extent;
      top_extent;	right_extent;	rear_extent;
      top_extent;	left_extent;	rear_extent;
      bottom_extent;	left_extent;	rear_extent;

      (* Object 1 colors *)
      green.(0); green.(1); green.(2); green.(3);
      green.(0); green.(1); green.(2); green.(3);
      green.(0); green.(1); green.(2); green.(3);
      green.(0); green.(1); green.(2); green.(3);

      blue.(0); blue.(1); blue.(2); blue.(3);
      blue.(0); blue.(1); blue.(2); blue.(3);
      blue.(0); blue.(1); blue.(2); blue.(3);
      blue.(0); blue.(1); blue.(2); blue.(3);

      red.(0); red.(1); red.(2); red.(3);
      red.(0); red.(1); red.(2); red.(3);
      red.(0); red.(1); red.(2); red.(3);

      grey.(0); grey.(1); grey.(2); grey.(3);
      grey.(0); grey.(1); grey.(2); grey.(3);
      grey.(0); grey.(1); grey.(2); grey.(3);

      brown.(0); brown.(1); brown.(2); brown.(3);
      brown.(0); brown.(1); brown.(2); brown.(3);
      brown.(0); brown.(1); brown.(2); brown.(3);
      brown.(0); brown.(1); brown.(2); brown.(3);

      (* Object 2 colors *)
      red.(0); red.(1); red.(2); red.(3);
      red.(0); red.(1); red.(2); red.(3);
      red.(0); red.(1); red.(2); red.(3);
      red.(0); red.(1); red.(2); red.(3);

      brown.(0); brown.(1); brown.(2); brown.(3);
      brown.(0); brown.(1); brown.(2); brown.(3);
      brown.(0); brown.(1); brown.(2); brown.(3);
      brown.(0); brown.(1); brown.(2); brown.(3);

      blue.(0); blue.(1); blue.(2); blue.(3);
      blue.(0); blue.(1); blue.(2); blue.(3);
      blue.(0); blue.(1); blue.(2); blue.(3);

      green.(0); green.(1); green.(2); green.(3);
      green.(0); green.(1); green.(2); green.(3);
      green.(0); green.(1); green.(2); green.(3);

      grey.(0); grey.(1); grey.(2); grey.(3);
      grey.(0); grey.(1); grey.(2); grey.(3);
      grey.(0); grey.(1); grey.(2); grey.(3);
      grey.(0); grey.(1); grey.(2); grey.(3);
     |]

  let indices =
    [|
      0l; 2l; 1l;
      3l; 2l; 0l;

      4l; 5l; 6l;
      6l; 7l; 4l;

      8l; 9l; 10l;
      11l; 13l; 12l;

      14l; 16l; 15l;
      17l; 16l; 14l;
     |]



end

module Tut13 = struct

  let title = "Tut 05 Base Vertex with Overlap"

  let strVertexShader = Tut12.strVertexShader

  let strFragmentShader = Tut12.strFragmentShader

  let count   = Tut12.count
  let tris    = Tut12.tris
  let indices =
    [|
      0; 2; 1;
      3; 2; 0;

      4; 5; 6;
      6; 7; 4;

      8; 9; 10;
      11; 13; 12;

      14; 16; 15;
      17; 16; 14;
     |]


end

module Tut14 = struct

  let title = "Tut 05 Depth Buffering"

  let strVertexShader = Tut12.strVertexShader

  let strFragmentShader = Tut12.strFragmentShader

  let count   = Tut12.count
  let tris    = Tut12.tris
  let indices = Tut13.indices

end

module Tut15 = struct

  let title = "Tut 05 Vertex Clipping"

  let strVertexShader = Tut12.strVertexShader

  let strFragmentShader = Tut12.strFragmentShader

  let count   = Tut12.count
  let tris    = Tut12.tris
  let indices = Tut13.indices

end

module Tut16 = struct

  let title = "Tut 05 Depth Clamping"

  let strVertexShader = Tut12.strVertexShader

  let strFragmentShader = Tut12.strFragmentShader

  let count   = Tut12.count
  let tris    = Tut12.tris
  let indices = Tut13.indices

end

module Tut17 = struct


  let strVertexShader = Tut12.strVertexShader

  let strFragmentShader = Tut12.strFragmentShader

  let title = "Tut 05 Depth Fighting"
  let green = [| 0.0; 1.0; 0.0; 1.0 |]
  let blue  = [| 0.0; 0.0; 1.0; 1.0 |]
  let red   = [| 1.0; 0.0; 0.0; 1.0 |]

  let z_offset = -0.5

  let tris =
    [|
      (* Front face positions *)
      -400.0;		 400.0;			0.0;
       400.0;		 400.0;			0.0;
       400.0;		-400.0;			0.0;
      -400.0;		-400.0;			0.0;

	(* Rear face positions *)
      -200.0;		 600.0;		       z_offset;
       600.0;		 600.0;		       z_offset;
       600.0;		-200.0;		       z_offset;
      -200.0;		-200.0;		       z_offset;

	(* Front face colors. *)
	green.(0); green.(1); green.(2); green.(3);
	green.(0); green.(1); green.(2); green.(3);
	green.(0); green.(1); green.(2); green.(3);
	green.(0); green.(1); green.(2); green.(3);

	(* Rear face colors. *)
	red.(0); red.(1); red.(2); red.(3);
	red.(0); red.(1); red.(2); red.(3);
	red.(0); red.(1); red.(2); red.(3);
	red.(0); red.(1); red.(2); red.(3);
     |]

  let indices =
    [|
      0; 1; 3;
      1; 2; 3;
      
      4; 5; 7;
      5; 6; 7;
     |]

  let count   = 8 (* vertices *)

end

module Tut18 = struct

  let strVertexShader = "
    #version 330

    layout(location = 0) in vec4 position;
    layout(location = 1) in vec4 color;

    smooth out vec4 theColor;

    uniform mat4 cameraToClipMatrix;
    uniform mat4 modelToCameraMatrix;

    void main()
    {
            vec4 cameraPos = modelToCameraMatrix * position;
            gl_Position = cameraToClipMatrix * cameraPos;
            theColor = color;
    }
    "

  let strFragmentShader = "
    #version 330

    smooth in vec4 theColor;

    out vec4 outputColor;

    void main()
    {
            outputColor = theColor;
    }
   "
  let count = 8

  let green = [| 0.0; 1.0; 0.0; 1.0 |]
  let blue  = [| 0.0; 0.0; 1.0; 1.0 |]
  let red   = [| 1.0; 0.0; 0.0; 1.0 |]
  let grey  = [| 0.8; 0.8; 0.8; 1.0 |]
  let brown = [| 0.5; 0.5; 0.0; 1.0 |]

  let tris = [|
	+1.0; +1.0; +1.0;
	-1.0; -1.0; +1.0;
	-1.0; +1.0; -1.0;
	+1.0; -1.0; -1.0;

	-1.0; -1.0; -1.0;
	+1.0; +1.0; -1.0;
	+1.0; -1.0; +1.0;
	-1.0; +1.0; +1.0;

      green.(0); green.(1); green.(2); green.(3);
      green.(0); green.(1); green.(2); green.(3);
      green.(0); green.(1); green.(2); green.(3);
      green.(0); green.(1); green.(2); green.(3);

      blue.(0); blue.(1); blue.(2); blue.(3);
      blue.(0); blue.(1); blue.(2); blue.(3);
      blue.(0); blue.(1); blue.(2); blue.(3);
      blue.(0); blue.(1); blue.(2); blue.(3);

      red.(0); red.(1); red.(2); red.(3);
      red.(0); red.(1); red.(2); red.(3);
      red.(0); red.(1); red.(2); red.(3);

      brown.(0); brown.(1); brown.(2); brown.(3);
      brown.(0); brown.(1); brown.(2); brown.(3);
      brown.(0); brown.(1); brown.(2); brown.(3);
      brown.(0); brown.(1); brown.(2); brown.(3);

      green.(0); green.(1); green.(2); green.(3);
      green.(0); green.(1); green.(2); green.(3);
      green.(0); green.(1); green.(2); green.(3);
      green.(0); green.(1); green.(2); green.(3);

      blue.(0); blue.(1); blue.(2); blue.(3);
      blue.(0); blue.(1); blue.(2); blue.(3);
      blue.(0); blue.(1); blue.(2); blue.(3);
      blue.(0); blue.(1); blue.(2); blue.(3);

      red.(0); red.(1); red.(2); red.(3);
      red.(0); red.(1); red.(2); red.(3);
      red.(0); red.(1); red.(2); red.(3);

      brown.(0); brown.(1); brown.(2); brown.(3);
      brown.(0); brown.(1); brown.(2); brown.(3);
      brown.(0); brown.(1); brown.(2); brown.(3);
      brown.(0); brown.(1); brown.(2); brown.(3);
       |]

  let indices = [|
	0; 1; 2;
	1; 0; 3;
	2; 3; 0;
	3; 2; 1;

	5; 4; 6;
	4; 5; 7;
	7; 6; 4;
	6; 7; 5;
       |]

end
