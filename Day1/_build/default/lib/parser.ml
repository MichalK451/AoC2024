let read_numbers file =
  let opened_file = open_in file in
    let rec read_line acc = 
      try
        let line = input_line opened_file in
          read_line (line::acc);
      with End_of_file -> Printer.print_num (string_of_int (Calibrator.evaluate_sum acc)) "sum.txt"
    in
    read_line [];
    close_in opened_file

let read_text file =
  let opened_file = open_in file in
    let rec read_line () = 
      try
        let line = input_line opened_file in
           Calibrator.find_digits line;
           read_line ();
      with End_of_file -> ()
    in 
    read_line ();
    read_numbers "output.txt";
    close_in opened_file