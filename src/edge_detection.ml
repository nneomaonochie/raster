open Core

let get_gx (gx, _) = gx;;
let get_gy (_,gy) = gy;;
let threshold = 0.4

(* transforms an image to showcase the edge lines *)
let transform image =
  let gray_image = Grayscale.transform image
  (* i want to try edge mapping, but we will ignore for now while i get used to it 
     
  1. Calculate horizontal (Gx) and vertical (Gy) gradient values for each pixel in the image.
    - Create a subimage with image splice, with x,y center and x-1, x + 1 (3 x 3 square)


  2. Calculate the final gradient magnitude for each pixel using the calculated and values.
  3. Set each pixel to black or white based on whether the magnitude exceeds a user-provided threshold
  *)

  (* intent: i want to make a subimage, and for each pixel in the subimage, i want to get the gx and gy vakye*)



  let find_gradient ~x ~y = 
    let grad_x = [[-1;0;1];[-2;0;2];[-1;0;1]] in
    let grad_y = [[-1;-2;-1];[0;0;0];[1;2;1]] in
    let old_val = Pixel.red (Image.get ~x ~y)
    let grad_x_val = old_val * grad_x[x][y] in
    let grad_y_val = old_val * grad_y[x][y] in
    (grad_x_val, grad_y_val)
  in
  let find_edges ~x ~y (pix : Pixel.t) = 
    (* rn we are ignoring border images *)
    if x > 0 && x < (Image.width image - 1) && y > 0 && y < (Image.height image - 1) then
    let sub_image = Image.slice ~x_start:(x - 1) ~x_end:(x + 1) ~y_start:(y - 1) ~y_end:(y + 1) in
    (* for each pixel in each sub image...*)
          let g_values : (int * int) = Array.mapi sub_image ~f:find_gradient
          let final_g = Float.sqrt (square (get_gx g_values) + square (get_gy g_values)) in
          if Float.(final_g > (Image.max_val image * threshold)) then Pixel.to_int (Image.max_val image) else 0
  in
  (* i think mapi is correct - we need to know the original afterall *)
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
