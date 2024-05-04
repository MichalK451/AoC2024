let match_digit character = 
  match character with
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | _ -> -1

let find_digits line =
  let rec char_list chars index =
    try (
      let ch = String.get line index in 
        if ch != '\n' then ch::(char_list chars (index + 1))
        else ch::[]
      )
    with Invalid_argument _ -> chars 
  in 
    let rec extract_numbers char_list numbers_list = 
      match char_list with
        | x::xs -> 
          let num = match_digit x in 
            if num != -1
            then extract_numbers xs (num::numbers_list) 
            else extract_numbers xs numbers_list
        | _ -> numbers_list
    in 
      let reversed_numbers_list = List.rev (extract_numbers (char_list [] 0) []) in
      let first_number = List.nth reversed_numbers_list 0 in
      let second_number = List.nth reversed_numbers_list (List.length reversed_numbers_list - 1) in
      let complete_number = (first_number * 10) + second_number in
      Printer.print_num (string_of_int (complete_number)) "output.txt"

let evaluate_sum string_list = 
  let rec sum_func_tail list acc = 
  match list with
    | x::xs -> sum_func_tail xs (acc + (int_of_string x))
    | _ -> acc
  in sum_func_tail string_list 0
