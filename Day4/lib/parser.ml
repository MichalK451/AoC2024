let parse input_file_string =
  let open_file = open_in input_file_string
  in
    let rec read_lines acc =
      try (
        let line = input_line open_file in
        read_lines (line::acc)
      )
      with End_of_file -> Manager.evaluate acc 
    in
      read_lines [];
      close_in open_file