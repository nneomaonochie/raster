open Core

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
      ~x_start:(random_x - w)
      ~x_end:random_x
      ~y_start:(random_y - h)
      ~y_end:random_y
  in
  let targets : Image.t list = [] in
  let targets = create_image_grid ~x_curr:0 ~y_curr:0 targets in
  (* will delete later, here while i pause and want it to compile*)
  let test = List.length targets in
  image
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
