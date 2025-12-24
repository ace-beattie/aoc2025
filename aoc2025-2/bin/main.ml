let puzzle_text = In_channel.with_open_text "input.txt" In_channel.input_all

let is_invalid num =
  let string_val = string_of_int num in
  let length = String.length string_val in
  if length mod 2 <> 0 then false
  else
    String.equal
      (String.sub string_val 0 (length / 2))
      (String.sub string_val (length / 2) (length / 2))

let rec invalid_in_range ?(accum = 0) (start : int) (ends : int) : int =
  if start > ends then accum
  else if is_invalid start then
    invalid_in_range (start + 1) ends ~accum:(accum + start)
  else invalid_in_range (start + 1) ends ~accum

let rec count_all lst =
  match lst with
  | [] -> 0
  | [ start; ends ] :: rest ->
      invalid_in_range (int_of_string start) (int_of_string ends)
      + count_all rest
  | _ :: rest -> count_all rest

(* Ranges, represented as a list of pairs of integers like [[1; 4]; [5; 8]] *)
let result =
  String.split_on_char ',' puzzle_text
  |> List.map (fun l -> String.split_on_char '-' l)
  |> count_all

let () = print_endline ("Result: " ^ string_of_int result)
