let puzzle_input = In_channel.with_open_text "input.txt" In_channel.input_all

let banks =
  puzzle_input |> String.split_on_char '\n'
  |> List.filter (fun s -> String.length s > 0)

type digit_info = { value : int; index : int }

(** Find the largest digit in a string [bank] *)
let rec find_largest_digit ?(init_index = 0)
    ?(current_largest = { value = Int.min_int; index = 0 }) bank =
  if init_index >= String.length bank then current_largest
  else
    let integer_value = Char.code bank.[init_index] - 48 in
    if integer_value > current_largest.value then
      find_largest_digit ~init_index:(init_index + 1)
        ~current_largest:{ value = integer_value; index = init_index }
        bank
    else find_largest_digit ~init_index:(init_index + 1) ~current_largest bank

(** Solve a single bank by finding the largest 12-digit number that can be
    formed.

    Recursively finds largest possible digit from left-to-right (allowing room
    for the remaining digits) *)
let rec solve_bank bank digits =
  if digits = 0 then ""
  else
    let largest =
      find_largest_digit (String.sub bank 0 (String.length bank - digits + 1))
    in
    string_of_int largest.value
    ^ solve_bank
        (String.sub bank (largest.index + 1)
           (String.length bank - (largest.index + 1)))
        (digits - 1)

let rec solve_all banks =
  match banks with
  | [] -> 0
  | bank :: rest -> int_of_string (solve_bank bank 12) + solve_all rest

let () = print_endline ("Total: " ^ string_of_int (solve_all banks))
