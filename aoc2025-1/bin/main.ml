let puzzle = In_channel.with_open_text "input.txt" In_channel.input_all

(* This overrides modulo to force a positive remainder e.g. -1 % 4 = 3, not -1 https://math.stackexchange.com/a/623454 *)
let (mod) x y = ((x mod y) + y) mod y

let instructions =
  String.split_on_char '\n' puzzle
  |> List.filter (fun s -> String.length s > 0)

let rec solve_puzzle (lines: string list) (curr: int) (acc: int): int =
  match lines with
  | [] -> acc
  | line :: rest ->
    let distance = int_of_string (String.sub line 1 (String.length line - 1)) in (* Extract distance from instruction *)
    let new_location = (if line.[0] == 'R' then curr + distance else curr - distance) mod 100 in (* Update current location after instruction *)
    solve_puzzle rest new_location (acc + (distance + (if line.[0] == 'R' || curr == 0 then curr else 100 - curr)) / 100)

let result = solve_puzzle instructions 50 0

let () = print_endline ("Result: " ^ (string_of_int result))
