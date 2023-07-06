open Core

(* takes coordinates and a pixel and blurs the pixel *)
let transform image ~radius =
  (* takes coordinates and a pixel and blurs the pixel by taking average of
     pixels *)
  let blur_pixels ~x ~y (pix : Pixel.t) =
    (* lets generically assign the 4 pts in the beginning*)
    let x_start = if x - radius < 0 then 0 else x - radius in
    let x_end =
      if x + radius >= Image.width image
      then Image.width image - 1
      else x + radius
    in
    let y_start = if y - radius < 0 then 0 else y - radius in
    let y_end =
      if y + radius >= Image.height image
      then Image.height image - 1
      else y + radius
    in
    let sub_image = Image.slice image ~x_start ~x_end ~y_start ~y_end in
    ignore pix;
    Image.mean_pixel sub_image
  in
  Image.mapi image ~f:blur_pixels
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
