let parse_raw file = 
  let opened_file = open_in file in
    let rec read_lines acc =
      try (
        let line = input_line opened_file in
        read_lines (line::acc)
      ) with End_of_file -> Manager.evaluate_game_outcome acc
    in 
    read_lines [];
    close_in opened_file