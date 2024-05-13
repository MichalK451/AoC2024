let extract_numbers raw_lines =
  let rec extract_numbers_internal raw_lines_internal acc =
    match raw_lines_internal with
      | line::rest -> 
          let extracted_numbers = List.nth (String.split_on_char ':' line) 1
          in 
          extract_numbers_internal rest (extracted_numbers::acc)
      | [] -> acc
  in
  extract_numbers_internal raw_lines [];;

let extract_winners_and_aquired raw_lines =
  let rec extract_winners_internal raw_lines_internal winners_acc aquired_acc =
    match raw_lines_internal with
      | line::rest -> 
          let splitted = String.split_on_char '|' line in
          let winners = List.nth splitted 0 in
          let aquired = List.nth splitted 1 in
          extract_winners_internal rest (winners::winners_acc) (aquired::aquired_acc)
      | [] -> winners_acc,aquired_acc
  in
  extract_winners_internal raw_lines [] [];;

let string_to_ints winners_list aquired_list =
  let rec split input_list_split acc =
    match input_list_split with
      | item::rest -> split rest ((String.split_on_char ' ' item) :: acc)
      | [] -> acc
  in
    let rec tidy input_list_tidy acc =
      match input_list_tidy with
        | item::rest -> tidy rest ((String.trim item)::acc)
        | [] -> acc
    in
      let cast input_list_cast =
        let rec cast_internal input_list_cast_internal acc =
          match input_list_cast_internal with
            | item::rest -> (
                try (
                  let read = int_of_string item 
                  in
                    cast_internal rest (read::acc)  
                )
                with (Failure _) -> cast_internal rest acc 
            )
            | [] -> acc 
        in cast_internal input_list_cast []
      in List.map cast (split (tidy winners_list []) []), List.map cast (split (tidy aquired_list []) []);;

let calculate_wins winning_list_list aquired_list_list =
  let list_size_to_winning lst =
    let square_power exp =
      let rec square_power_internal exponent acc =
      if exponent > 0 then square_power_internal (exponent - 1) (acc * 2) else acc
      in square_power_internal exp 1
    in
    let rec list_size_to_winning_internal lst_internal acc =
      match lst_internal with
        | x::xs -> if x = 1 then list_size_to_winning_internal xs (acc + 1) else list_size_to_winning_internal xs acc
        | [] -> if acc = 2 then 2 else if acc = 1 then 1 else if acc = 0 then 0 else square_power (acc - 1)
    in list_size_to_winning_internal lst 0
  in
  let compare_to_winner winning_number aquired_numbers_list =
    let rec compare_to_winner_internal winning_number_internal aquired_numbers_list =
      match aquired_numbers_list with
        | number::rest -> 
            if number = winning_number_internal 
            then (Printf.printf "%s%d%s" "Match!: " winning_number_internal "\n"; [1])
            else compare_to_winner_internal winning_number_internal rest
        | [] -> []
    in compare_to_winner_internal winning_number aquired_numbers_list
  in
  let compare_for_all_winners winners aquired =
    let rec compare_for_all_winners_internal winners_internal aquired_internal acc_internal =
      match winners_internal with
        | winner::rest_winners -> compare_for_all_winners_internal rest_winners aquired_internal ((compare_to_winner winner aquired_internal) @ acc_internal)
        | [] -> Printf.printf "%s%d%s" "Adding: " (list_size_to_winning acc_internal) "\n"; list_size_to_winning acc_internal
    in compare_for_all_winners_internal winners aquired []
  in
  let unpack_lists packed_winners packed_aquired =
    let rec unpack_lists_internal packed_winners_internal packed_aquired_internal acc =
      match packed_winners_internal,packed_aquired_internal with
        | unpacked_winners::rest_winners,unpacked_aquired::rest_aquired ->
          unpack_lists_internal rest_winners rest_aquired ((compare_for_all_winners unpacked_winners unpacked_aquired) :: acc)
        | [],[] -> List.fold_left (fun x y-> Printf.printf "%s%d%s%d%s" "Adding: " x " and " y "\n"; x + y) 0 acc
        | _,_ -> raise (Failure "Number of lists mismatch!")
    in unpack_lists_internal packed_winners packed_aquired []
  in unpack_lists winning_list_list aquired_list_list;;


let evaluate games = 
  let winners,aquired = extract_winners_and_aquired (extract_numbers games)
  in 
    let winners_int, aquired_int = string_to_ints winners aquired
    in Printer.print_num (string_of_int (calculate_wins winners_int aquired_int)) "output.txt";;