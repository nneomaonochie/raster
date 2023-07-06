open Core

(* takes in a pixel RGB value and removes bits*)
let binary_masking_val = 3

let transform image =
  (* take in an image - take in a pixel, convert to binary, throw out the
     first six bits per RGB - with the last two bits (either 0, 1, 2, or 3),
     shift the bits six places - set the new values as the pixel *)
  let lower_bits (pix : Pixel.t) =
    (* takes in a pixel and removes the 6 higher bits of a pixel value *)
    let alter_value pix_val =
      (* uses the masking number to ensure only the rightmost two bits are
         counted*)
      let val_binary = Int.( land ) pix_val binary_masking_val in
      Int.shift_left val_binary 6
    in
    let new_red = alter_value (Pixel.red pix) in
    let new_green = alter_value (Pixel.green pix) in
    let new_blue = alter_value (Pixel.blue pix) in
    let new_pix : Pixel.t = new_red, new_green, new_blue in
    new_pix
  in
  Image.map image ~f:lower_bits
;;

let command =
  Command.basic
    ~summary:"Finds hidden images"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm image ~filename:"mystery.ppm"]
;;
