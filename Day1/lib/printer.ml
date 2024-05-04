let print_num num file =
  let opened_file = open_out_gen [Open_append] 0o666 file in
    Printf.fprintf opened_file "%s%s" num "\n";
    close_out opened_file

(* let print_numbers num1 num2 = 
  let opened_file = open_out_gen [Open_append] 0o666 "output.txt" in 
    Printf.fprintf opened_file "%s%s%s" num1 num2 "\n";
    close_out opened_file in (); *)