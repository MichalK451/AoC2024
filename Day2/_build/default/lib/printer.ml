let print_num num file =
  let opened_file = open_out_gen [Open_append] 0o666 file in
    Printf.fprintf opened_file "%s%s" num "\n";
    close_out opened_file