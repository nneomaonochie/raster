open Core

let gray_value = 0.5
let error_factor = 16.0

(* takes in the new_val, error tuplets and maeks a giant tuplet of errors *)
let create_error_tuple ~red:(_, f1) ~green:(_, f2) ~blue:(_, f3)
  : float * float * float
  =
  f1, f2, f3
;;

let create_new_pix ~red:(i1, _) ~green:(i2, _) ~blue:(i3, _) : Pixel.t =
  i1, i2, i3
;;

let red_err (f1, _, _) = f1
let green_err (_, f2, _) = f2
let blue_err (_, _, f3) = f3

(* distributes errors to adjacent pixels *)
let distribute_errors x y all_errors image : Image.t =
  (* these will be the base error *)
  let red_error = red_err all_errors /. error_factor in
  let green_error = green_err all_errors /. error_factor in
  let blue_error = blue_err all_errors /. error_factor in
  let error_directions ~new_x ~new_y ~multiply_error =
    if new_x >= 0
       && new_x < Image.width image
       && new_y >= 0
       && new_y < Image.height image
    then (
      let pix : Pixel.t =
        ( Float.to_int (multiply_error *. red_error)
          + Pixel.red (Image.get image ~x:new_x ~y:new_y)
        , Float.to_int (multiply_error *. green_error)
          + Pixel.green (Image.get image ~x:new_x ~y:new_y)
        , Float.to_int (multiply_error *. blue_error)
          + Pixel.blue (Image.get image ~x:new_x ~y:new_y) )
      in
      Image.set image ~x:new_x ~y:new_y pix)
  in
  (* east *)
  error_directions ~new_x:(x + 1) ~new_y:y ~multiply_error:7.0;
  (* southwest*)
  error_directions ~new_x:(x - 1) ~new_y:(y + 1) ~multiply_error:3.0;
  (* south *)
  error_directions ~new_x:x ~new_y:(y + 1) ~multiply_error:5.0;
  (* southeast *)
  error_directions ~new_x:(x + 1) ~new_y:(y + 1) ~multiply_error:1.0;
  image
;;

(* takes in an image and creates the image in a dithered effect *)
let transform image =
  let dither_pixels ~x ~y image (pix : Pixel.t) : Image.t =
    let dither_with_colors old_val : int * float =
      let pix_percentage =
        Int.to_float old_val /. Int.to_float (Image.max_val image)
      in
      (* depending on what the pixel's value is, its value is reset to
         image's max value (white) or 0 (black) *)
      let new_val =
        if Float.(pix_percentage > gray_value)
        then Image.max_val image
        else 0
      in
      let error = Int.to_float old_val -. Int.to_float new_val in
      new_val, error
    in
    let new_red = dither_with_colors (Pixel.red pix) in
    let new_green = dither_with_colors (Pixel.green pix) in
    let new_blue = dither_with_colors (Pixel.blue pix) in
    let new_pix =
      create_new_pix ~red:new_red ~green:new_green ~blue:new_blue
    in
    let all_errors =
      create_error_tuple ~red:new_red ~green:new_green ~blue:new_blue
    in
    Image.set image ~x ~y new_pix;
    distribute_errors x y all_errors image
  in
  Image.foldi image ~init:image ~f:dither_pixels
;;

(* let pix = if Float.compare pix_percentage gray_value > 0 then Pixel.of_int
   (Image.max_val image) else Pixel.of_int 0 *)

let command =
  Command.basic
    ~summary:"Dither an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
