let puzzle_text =
  In_channel.with_open_text "input.txt" In_channel.input_all |> String.trim

let rec all_subs ?(init = 1) str =
  let rec subs_of_len len str =
    if String.length str mod len <> 0 then []
    else if String.length str = 0 then []
    else
      let sub = String.sub str 0 len in
      sub :: subs_of_len len (String.sub str len (String.length str - len))
  in

  if init > String.length str / 2 then []
  else subs_of_len init str :: all_subs ~init:(init + 1) str

let is_invalid num =
  let str = string_of_int num in
  let all_subs = all_subs str in
  let rec sub_is_invalid sub =
    match sub with
    | [] -> false
    | _ :: [] -> true
    | sub :: rest ->
        if String.equal sub (List.hd rest) then sub_is_invalid rest else false
  in

  let is_match =
    List.map sub_is_invalid all_subs |> List.exists (fun x -> x = true)
  in
  let () = if is_match then print_endline ("Match: " ^ str) else () in
  is_match

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
