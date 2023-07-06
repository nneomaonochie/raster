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

(*distributes erros to adjacent pixels*)
let distribute_errors x y all_errors image : Image.t =
  (* these will be the base error *)
  let red_error = red_err all_errors /. error_factor in
  let green_error = green_err all_errors /. error_factor in
  let blue_error = blue_err all_errors /. error_factor in
  (* distributing east *)
  if x + 1 < Image.width image
  then (
    (* adding error to the value on the right *)
    let pix_east =
      ( Float.to_int (7.0 *. red_error)
        + Pixel.red (Image.get image ~x:(x + 1) ~y)
      , Float.to_int (7.0 *. green_error)
        + Pixel.green (Image.get image ~x:(x + 1) ~y)
      , Float.to_int (7.0 *. blue_error)
        + Pixel.blue (Image.get image ~x:(x + 1) ~y) )
    in
    Image.set image ~x:(x + 1) ~y pix_east);
  (* distributing southwest*)
  if x - 1 >= 0 && y + 1 < Image.height image
  then (
    let pix_southwest =
      ( Float.to_int (3.0 *. red_error)
        + Pixel.red (Image.get image ~x:(x - 1) ~y:(y + 1))
      , Float.to_int (3.0 *. green_error)
        + Pixel.green (Image.get image ~x:(x - 1) ~y:(y + 1))
      , Float.to_int (3.0 *. blue_error)
        + Pixel.blue (Image.get image ~x:(x - 1) ~y:(y + 1)) )
    in
    Image.set image ~x:(x - 1) ~y:(y + 1) pix_southwest);
  (* do the rest of errors*)

  (* distributing south*)
  if y + 1 < Image.height image
  then (
    let pix_south =
      Pixel.of_int
        (Float.to_int (5.0 *. base_error)
         + Pixel.red (Image.get image ~x ~y:(y + 1)))
    in
    Image.set image ~x ~y:(y + 1) pix_south);
  (* distributing southeast *)
  if x + 1 < Image.width image && y + 1 < Image.height image
  then (
    let pix_southeast =
      Pixel.of_int
        (Float.to_int base_error
         + Pixel.red (Image.get image ~x:(x + 1) ~y:(y + 1)))
    in
    Image.set image ~x:(x + 1) ~y:(y + 1) pix_southeast);
  image
;;

(* takes in an image and creates the image in a dithered effect *)
let transform image =
  (* converts the image to grayscale to more easily dither it *)
  (*let gray_image = Grayscale.transform image in *)
  let dither_pixels ~x ~y gray_image (pix : Pixel.t) : Image.t =
    (* new pix value * error *)
    let dither_with_colors old_val : int * float =
      let pix_percentage =
        Int.to_float old_val /. Int.to_float (Image.max_val gray_image)
      in
      (* depending on what the pixel's value is, its value is reset to 255
         (white) or 0 (black) *)
      let new_val =
        if Float.compare pix_percentage gray_value > 0
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
    Image.set gray_image ~x ~y new_pix;
    (* ignore all_errors; gray_image *)
    (* let error = Int.to_float pix_old_val -. Int.to_float (Pixel.red pix)
       in *)
    distribute_errors x y all_errors gray_image
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
