open Raytracing

let clamp_colour (v: Vect.vect) : Vect.vect =
  let fr, fg, fb = v in
  ((fr *. 255.0) |> Float.max 0.0 |> Float.min 255.0,
   (fg *. 255.0) |> Float.max 0.0 |> Float.min 255.0,
   (fb *. 255.0) |> Float.max 0.0 |> Float.min 255.0)

let gamma_correct (gamma: float) (v: Vect.vect) : Vect.vect =
  let fr, fg, fb = v in
  (Float.pow fr gamma, Float.pow fg gamma, Float.pow fb gamma)
  
let main () =
  let img = Image.create_rgb Params.width Params.height in
  let scene = Scene_loader.load_scene "scene.toml" in
  let camera = Camera.new_cam (0.0, 2.0, 0.0) (0.0, 0.0, 1.0) (0.0, 1.0, 0.0) 1.04 in
  let () = print_string "Scene loaded. Rendering...\n" in
  let () =
    for i = 0 to (Params.height - 1) do
      for j = 0 to (Params.width - 1) do
        let ray = Camera.ray camera i j in
        let c = Scene.get_colour ray scene Params.rendering_depth in
        let im_colour = c |> (gamma_correct 0.45) |> clamp_colour in
        let (r, g, b) = im_colour in
        Image.write_rgb img j i (int_of_float r) (int_of_float g) (int_of_float b)
      done
    done in
    let () = print_string "Rendering completed. Saving...\n" in
    let () = ImageLib_unix.writefile Params.filename img in
    print_string ("Image saved to '" ^ Params.filename ^ "'\n")

let () = main ()