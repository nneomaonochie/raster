open Core

let threshold = 0.4
let square x = x * x

(* transforms an image to showcase the edge lines *)
let transform image =
  let gray_image = Grayscale.transform image in
  let find_edges ~x ~y (pix : Pixel.t) =
    ignore pix;
    (* we are ignoring border images *)
    if x > 0
       && x < Image.width gray_image - 1
       && y > 0
       && y < Image.height gray_image - 1
    then (
      (* uses the Sobel operator kernels to get the gradient x and y
         values *)
      let grad_x_val =
        (Pixel.red (Image.get gray_image ~x:(x - 1) ~y:(y - 1)) * -1)
        + Pixel.red (Image.get gray_image ~x:(x + 1) ~y:(y - 1))
        + (Pixel.red (Image.get gray_image ~x:(x - 1) ~y) * -2)
        + (Pixel.red (Image.get gray_image ~x:(x + 1) ~y) * 2)
        + (Pixel.red (Image.get gray_image ~x:(x - 1) ~y:(y + 1)) * -1)
        + Pixel.red (Image.get gray_image ~x:(x + 1) ~y:(y - 1))
      in
      let grad_y_val =
        (Pixel.red (Image.get gray_image ~x:(x - 1) ~y:(y - 1)) * -1)
        + (Pixel.red (Image.get gray_image ~x ~y:(y - 1)) * -2)
        + (Pixel.red (Image.get gray_image ~x:(x + 1) ~y:(y - 1)) * -1)
        + Pixel.red (Image.get gray_image ~x:(x - 1) ~y:(y + 1))
        + (Pixel.red (Image.get gray_image ~x ~y:(y + 1)) * 2)
        + Pixel.red (Image.get gray_image ~x:(x + 1) ~y:(y + 1))
      in
      let final_g =
        Float.sqrt (Int.to_float (square grad_x_val + square grad_y_val))
      in
      let new_pix_value =
        if Float.(
             final_g > Int.to_float (Image.max_val gray_image) *. threshold)
        then Image.max_val gray_image
        else 0
      in
      Pixel.of_int new_pix_value)
    else Pixel.of_int 0
  in
  Image.mapi gray_image ~f:find_edges
;;

let command =
  Command.basic
    ~summary:"Only shows a photo's edges"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edge.ppm")]
;;
