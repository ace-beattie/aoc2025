let puzzle_input = In_channel.with_open_text "input.txt" In_channel.input_all

let banks =
  puzzle_input |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s > 0)

let rec find_largest_digit ?(init_index = 0)
    ?(current_largest = (Int.min_int, 0)) bank stop_at =
  if init_index >= stop_at then current_largest
  else
    let integer_value = Char.code bank.[init_index] - 48 in
    if integer_value > fst current_largest then
      find_largest_digit ~init_index:(init_index + 1)
        ~current_largest:(integer_value, init_index)
        bank stop_at
    else
      find_largest_digit ~init_index:(init_index + 1) ~current_largest bank
        stop_at

let solve_bank bank =
  let largest_ten = find_largest_digit bank (String.length bank - 1) in
  let largest_one =
    find_largest_digit
      ~init_index:(snd largest_ten + 1)
      bank (String.length bank)
  in
  string_of_int (fst largest_ten) ^ string_of_int (fst largest_one)
  |> int_of_string

let rec solve_all banks =
  match banks with [] -> 0 | bank :: rest -> solve_bank bank + solve_all rest

let () = print_endline ("Total: " ^ string_of_int (solve_all banks))
