open Core

(* takes coordinates and a pixel and blurs the pixel *)
let transform image ~radius =
  (* takes coordinates and a pixel and blurs the pixel by taking average of
     pixels *)
  let blur_pixels ~x ~y (pix : Pixel.t) =
    if x - radius < 0
       || x + radius >= Image.width image
       || y - radius < 0
       || y + radius >= Image.height image
    then pix
    else (
      let sub_image =
        Image.slice
          image
          ~x_start:(x - radius)
          ~x_end:(x + radius)
          ~y_start:(y - radius)
          ~y_end:(y + radius)
      in
      Image.mean_pixel sub_image)
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
