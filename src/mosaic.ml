open Core

let square (x : int) : float = Int.to_float x *. Int.to_float x
let float_mse (f, _) = f
let subimage_mse (_, i) = i

(* transforms an image to be a mosaic version of itself *)
let transform image ~w ~h ~moves : Image.t =
  (* first we must pick a random region*)
  let rec create_image_grid ~x_curr ~y_curr (targets : Image.t list)
    : Image.t list
    =
    (* base case: x axis AND y axis is out of bounds*)
    if x_curr + w < Image.width image || y_curr + h < Image.height image
    then (
      let x_end =
        if x_curr + w >= Image.width image
        then Image.width image - 1
        else x_curr + w
      in
      let y_end =
        if y_curr + h >= Image.height image
        then Image.height image - 1
        else y_curr + h
      in
      let targets =
        targets
        @ [ Image.slice image ~x_start:x_curr ~x_end ~y_start:y_curr ~y_end ]
      in
      let x_curr = if x_end = Image.width image - 1 then 0 else x_curr in
      let y_curr =
        if x_end = Image.width image - 1 then y_curr + h else y_curr
      in
      create_image_grid ~x_curr:(x_curr + w) ~y_curr targets
      (* the case both x and y is about to be out of bounds *))
    else (
      let targets =
        targets
        @ [ Image.slice
              image
              ~x_start:x_curr
              ~x_end:(Image.width image)
              ~y_start:y_curr
              ~y_end:(Image.height image)
          ]
      in
      targets)
  in
  let random_x = Random.int (Image.width image - w) + w in
  let random_y = Random.int (Image.height image - h) + h in
  let region1 =
    Image.slice
      image
      ~x_start:(random_x - w)
      ~x_end:random_x
      ~y_start:(random_y - h)
      ~y_end:random_y
  in
  let targets : Image.t list = [] in
  (* targets is a list of image regions sliced by the user's width and height
     values *)
  let targets = create_image_grid ~x_curr:0 ~y_curr:0 targets in
  (* a smaller MSE means a higher similarity *)
  let smallest_MSE : float * Image.t = Float.max_value, region1 in
  let find_mean_square_errors
    (smallest_MSE : float * Image.t)
    (sub_image : Image.t)
    =
    let mse_equation ~x ~y sum (pix : Pixel.t) =
      (* region 1 - targets_region *)
      sum +. square (Pixel.red (Image.get region1 ~x ~y) - Pixel.red pix)
    in
    let sum = 0.0 in
    let check_value =
      Image.foldi sub_image ~init:sum ~f:mse_equation
      *. (1.0 /. Int.to_float w)
      *. (1.0 /. Int.to_float h)
    in
    let smallest_MSE =
      if Float.(check_value < float_mse smallest_MSE)
      then check_value, sub_image
      else smallest_MSE
    in
    smallest_MSE
  in
  (* make a method so i can extrapolate the Image from the touple *)
  let region2 =
    List.fold targets ~init:smallest_MSE ~f:find_mean_square_errors
  in
  let region2 : Image.t = subimage_mse region2 in
  (* we need to now swap pixels *)

  (* let create_mosaic moves -- repeat the above steps n times*)
  region2
;;

let command =
  Command.basic
    ~summary:"Create a mosaic of an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;
