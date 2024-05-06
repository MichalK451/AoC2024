type game_raw = Game_raw of int * string
type game_outcome = {id : int; outcome : string list}

let extract_game_raw line =
  let splitted = String.split_on_char ':' line in
  let id_substr = List.nth splitted 0 in
  let id = int_of_string (List.nth (String.split_on_char ' ' id_substr) 1) in
  let outcome = List.nth splitted 1 in Game_raw(id, outcome)

let extract_outcome outcome_line =
  match outcome_line with
    | Game_raw(_, raw) -> String.split_on_char ',' raw

let sum_truths_index bool_list =
  let rec evaluate_sum lst acc index =
    match lst with
      | flag::rest -> if flag then evaluate_sum rest (acc + index) (index + 1) else evaluate_sum rest acc (index + 1)
      | [] -> acc
  in evaluate_sum bool_list 0 1

let replace_semicolons ch1 =
  if ch1 = ';' then ',' else ch1  

let evaluate_game_outcome outcome_list =
  let rec extract_game_raws lst acc =
    match lst with 
      | line::rest -> extract_game_raws rest ((extract_game_raw line)::acc)
      | [] -> acc
  in 
    let rec tidy_game_raws (lst : game_raw list) acc = 
      match lst with
      | outcome::rest -> (
        match outcome with
          | Game_raw (id, raw) -> tidy_game_raws rest (Game_raw (id, (String.map replace_semicolons raw))::acc)
      )
      | [] -> acc
    in 
      let rec create_game_outcomes lst acc =
        match lst with 
          | outcome::rest -> create_game_outcomes rest ((extract_outcome outcome)::acc)
          | [] -> acc
      in
        let rec game_outcomes_to_bool lst =
          match lst with
            | tuple::rest -> (
              let tuple_list = String.split_on_char ' ' tuple in
              let amount = int_of_string (List.nth tuple_list 1) in
              let color = List.nth tuple_list 2 in 
              let trimmed_color = String.trim color in
              match trimmed_color with
                | "blue" -> if amount > 14 then false else game_outcomes_to_bool rest
                | "red" -> if amount > 12 then false else game_outcomes_to_bool rest
                | "green" -> if amount > 13 then false else game_outcomes_to_bool rest
                | _ -> true
            )
          | [] -> true
        in 
          let rec create_bool_list lst acc =
            match lst with
              | string_list::rest -> create_bool_list rest ((game_outcomes_to_bool string_list)::acc)
              | [] -> List.rev (acc)
          in 
            let result = sum_truths_index (create_bool_list (create_game_outcomes (tidy_game_raws (extract_game_raws outcome_list []) []) []) [])
            in Printer.print_num (string_of_int result) "output.txt"