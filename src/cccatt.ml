let parse_file f =
  let sin =
    let fi = open_in f in
    let flen = in_channel_length fi in
    let buf = Bytes.create flen in
    really_input fi buf 0 flen;
    close_in fi;
    buf
  in
  Prover.parse (Bytes.to_string sin)
 
let usage = "cccatt [options] [file]"

let () =
  Printexc.record_backtrace true;
  let files = ref [] in
  Arg.parse
    []
    (fun s -> files := s::!files)
    usage;
  List.iter (fun f -> Lang.exec [] [] (parse_file f)) !files
