open Core

let square (x : int) : float = Int.to_float x *. Int.to_float x
let float_mse (f, _) = f
let remove_float (_, i) : Image.t * int * int = i
let get_subimage (image, _, _) : Image.t = image
let get_x (_, x, _) : int = x
let get_y (_, _, y) : int = y

(* transforms an image to be a mosaic version of itself *)
let transform image ~w ~h ~moves : Image.t =
  let create_mosaic ~w ~h image =
    let rec create_image_grid
      ~x_curr
      ~y_curr
      (targets : (Image.t * int * int) list)
      : (Image.t * int * int) list
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
          @ [ ( Image.slice
                  image
                  ~x_start:x_curr
                  ~x_end
                  ~y_start:y_curr
                  ~y_end
              , x_curr
              , y_curr )
            ]
        in
        let x_curr = if x_end = Image.width image - 1 then 0 else x_curr in
        let y_curr =
          if x_end = Image.width image - 1 then y_curr + h else y_curr
        in
        create_image_grid ~x_curr:(x_curr + w) ~y_curr targets
        (* the case both x and y is about to be out of bounds *))
      else (
        (* the x, y coordinates correspond to the topmost left corner *)
        let targets =
          targets
          @ [ ( Image.slice
                  image
                  ~x_start:x_curr
                  ~x_end:(Image.width image)
                  ~y_start:y_curr
                  ~y_end:(Image.height image)
              , x_curr
              , y_curr )
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
    let targets : (Image.t * int * int) list = [] in
    (* targets is a list of image regions sliced by the user's width and
       height values *)
    let targets = create_image_grid ~x_curr:0 ~y_curr:0 targets in
    (* a smaller MSE means a higher similarity *)
    let smallest_MSE : float * (Image.t * int * int) =
      Float.max_value, (region1, random_x, random_y)
    in
    let find_mean_square_errors
      (smallest_MSE : float * (Image.t * int * int))
      (sub_image : Image.t * int * int)
      =
      let mse_equation ~x ~y sum (pix : Pixel.t) =
        (* region 1 - targets_region *)
        sum +. square (Pixel.red (Image.get region1 ~x ~y) - Pixel.red pix)
      in
      let sum = 0.0 in
      let check_value =
        Image.foldi (get_subimage sub_image) ~init:sum ~f:mse_equation
        *. (1.0 /. Int.to_float w)
        *. (1.0 /. Int.to_float h)
      in
      let smallest_MSE =
        if Float.(check_value < float_mse smallest_MSE)
        then check_value, sub_image
        else smallest_MSE
      in
      smallest_MSE (* put in an x, y so you know *)
    in
    (* make a method so i can extrapolate the Image from the touple *)
    let region2 =
      List.fold targets ~init:smallest_MSE ~f:find_mean_square_errors
    in
    let region2 : Image.t * int * int = remove_float region2 in
    (* we need to now swap pixels *)
    let swap_pixels ~x ~y (pix : Pixel.t) =
      let og_region1_x_coord = x + random_x in
      let og_region1_y_coord = y + random_y in
      let og_region2_x_coord = x + get_x region2 in
      let og_region2_y_coord = y + get_y region2 in
      let og_region1_pix =
        Image.get image ~x:og_region1_x_coord ~y:og_region1_y_coord
      in
      let og_region2_pix =
        Image.get image ~x:og_region2_x_coord ~y:og_region2_y_coord
      in
      (* swapping positions *)
      Image.set
        image
        ~x:og_region1_x_coord
        ~y:og_region1_y_coord
        og_region2_pix;
      Image.set
        image
        ~x:og_region2_x_coord
        ~y:og_region2_y_coord
        og_region1_pix;
      pix
    in
    (* its teh ENTIRE image with region1 SWAPPED with region two*)
    (* swapped_image isnt really going to do anything *)
    let s = Image.mapi region1 ~f:swap_pixels in
    (* let create_mosaic moves -- repeat the above steps n times*)
    ignore s;
    image
  in
  Fn.apply_n_times ~n:moves (create_mosaic ~w ~h) image
;;

(* divides an image into regions with the width and height given by the
   user*)

(* 1. i go through region 1 and get x,y 2. i add x,y to my RANDOM x,y
   (absolute poisition of region in OG image 3. add x,t to region2 OG
   position - now you know two coordinates, so swap them *)

let command =
  Command.basic
    ~summary:"Create a mosaic of an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and w = flag "width" (required Command.Param.int) ~doc:"user's width"
      and h =
        flag "height" (required Command.Param.int) ~doc:"user's height "
      and moves =
        flag
          "moves"
          (required Command.Param.int)
          ~doc:"number of times a repeat"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform ~w ~h ~moves in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;
