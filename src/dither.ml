open Core

let gray_value = 0.5
(* let error_factor = 16.0 *)

(* let distribute_errors x y error image : Image.t = let base_error = error
   /. error_factor in (* distributing east *) if x + 1 < Image.width image
   then ( (* adding error to the value on the right *) let pix_east =
   Pixel.of_int (Float.to_int (7.0 *. base_error) + Pixel.red (Image.get
   image ~x:(x + 1) ~y)) in Image.set image ~x:(x + 1) ~y pix_east); (*
   distributing southwest*) if x - 1 >= 0 && y + 1 < Image.height image then
   ( let pix_southwest = Pixel.of_int (Float.to_int (3.0 *. base_error) +
   Pixel.red (Image.get image ~x:(x - 1) ~y:(y + 1))) in Image.set image
   ~x:(x - 1) ~y:(y + 1) pix_southwest); (* distributing south*) if y + 1 <
   Image.height image then ( let pix_south = Pixel.of_int (Float.to_int (5.0
   *. base_error) + Pixel.red (Image.get image ~x ~y:(y + 1))) in Image.set
   image ~x ~y:(y + 1) pix_south); (* distributing southeast *) if x + 1 <
   Image.width image && y + 1 < Image.height image then ( let pix_southeast =
   Pixel.of_int (Float.to_int base_error + Pixel.red (Image.get image ~x:(x +
   1) ~y:(y + 1))) in Image.set image ~x:(x + 1) ~y:(y + 1) pix_southeast);
   image ;; *)

(* takes in an image and creates the image in a dithered effect *)
let transform image =
  (* converts the image to grayscale to more easily dither it *)
  let gray_image = Grayscale.transform image in
  let dither_pixels ~x ~y gray_image (pix : Pixel.t) : Image.t =
    let pix_old_val =
      Int.to_float (Pixel.red pix) /. Int.to_float (Image.max_val gray_image)
    in
    (* depending on what the pixel's value is, its value is reset to 255
       (white) or 0 (black) *)
    let pix =
      if Float.compare pix_old_val gray_value > 0
      then Pixel.of_int Image.max_val image
      else Pixel.of_int Image.max_val image
    in
    print_s
      [%message "new pix val" (Pixel.to_string pix) (x : int) (y : int)];
    (* im both setting and returning the pixel? why? *)
    Image.set gray_image ~x ~y pix;
    (* error is wrong, fix later*)
    let error = Int.to_float (Pixel.red pix) -. pix_old_val in
    ignore error;
    gray_image
    (*distribute_errors x y error gray_image*)
  in
  (* mayyyybe use same image? idk if thats sound *)

  (* the image being returned may not be the one i was changing*)

  (* use map, but use a different algo - get errors from adjacent pixels and
     then add those errors to those pixels (ex: pixel to east error is 0.5,
     so set east value to east value + 0.5)*)
  Image.foldi gray_image ~init:gray_image ~f:dither_pixels
;;

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
