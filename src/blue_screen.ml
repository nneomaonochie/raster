open Core

(* assume that the background image will always be larger than the foreground
   image*)
let transform ~foreground ~background =
  (* takes in a foreground pixel and returns a background pixel if
     significantly blue, else returns the same pixel *)
  let combine_bluescreen ~x ~y (pix_fg : Pixel.t) =
    if Pixel.blue pix_fg > Pixel.red pix_fg + Pixel.green pix_fg
    then Image.get background ~x ~y
    else pix_fg
  in
  Image.mapi foreground ~f:combine_bluescreen
;;

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;
