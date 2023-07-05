open Core

(* transforms an image to be a grayscale version of itself *)
let transform image =
  (* takes in a pixel and returns the pixel in a grayscale tone*)
  let convert_to_grayscale (pix : Pixel.t) =
    let gray_value =
      (Pixel.red pix + Pixel.green pix + Pixel.blue pix) / 3
    in
    let new_pix : Pixel.t = gray_value, gray_value, gray_value in
    new_pix
  in
  Image.map image ~f:convert_to_grayscale
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;
