open Images
open Raytracing

let rgb_of_vect (v: Vect.vect) =
  let fr, fg, fb = v in
  {r = int_of_float fr;
   g = int_of_float fg;
   b = int_of_float fb;}

let clamp_colour (v: Vect.vect) : Vect.vect =
  let fr, fg, fb = v in
  (Float.min 255.0 (Float.max (fr *. 255.0) 0.0),
   Float.min 255.0 (Float.max (fg *. 255.0) 0.0),
   Float.min 255.0 (Float.max (fb *. 255.0) 0.0))

let gamma_correct (gamma: float) (v: Vect.vect) : Vect.vect =
  let fr, fg, fb = v in
  (Float.pow fr gamma, Float.pow fg gamma, Float.pow fb gamma)
  
let main () =
  let img = Rgb24.create Params.width Params.height in
  let t = Sphere.sphere (0.0, 0.0, 10.0) 2.0 Lambertian (0.5, 0.5, 0.5) in
  let pl = Light.point_light Vect.zero 2000.0 in
  let scene: Typedefs.scene = ([], []) |>
  (Scene.add_geo t) |> (Scene.add_light pl) in
  let camera = Camera.new_cam (0.0, 0.0, 0.0) (0.0, 0.0, 1.0) (0.0, 1.0, 0.0) 1.04 in
  let () = print_string "Scene loaded. Rendering...\n" in
  let () =
    for i = 0 to (Params.height - 1) do
      for j = 0 to (Params.width - 1) do
        let ray = Camera.ray camera i j in
        let c = Scene.get_colour ray scene Params.rendering_depth in
        let im_colour = c |> (gamma_correct 0.45) |> clamp_colour |> rgb_of_vect in
        Rgb24.set img j i im_colour
      done
    done in
    let () = print_string "Rendering completed. Saving...\n" in
    let () = Png.save Params.filename [] (Rgb24(img)) in
    print_string ("Image saved to '" ^ Params.filename ^ "'\n")

let () = main ()