let debug = ref false

let options = [ ("-debug", Arg.Set debug, "Debug") ]

let files = ref []

let parse f = files := f :: !files

let usage = "Usage: ./test-runner.exe [options] [files]"

let _ =
  Arg.parse options parse usage;
  match !files with
  | [] -> Printf.eprintf "No files specified"
  | _ ->
      List.iter
        (fun f ->
          if Sys.file_exists f then (
            let _ = Format.eprintf "File: %s\n" f in
            Format.pp_print_flush Format.err_formatter ();
            let _ = Claml.Clang.initialize ~debug:!debug () in
            let tu =
              Claml.Clang.TranslationUnit.parse_file [| Sys.argv.(1) |]
            in
            Format.printf "%a" Claml.Clang.TranslationUnit.pp tu)
          else Printf.eprintf "%s not found\n" Sys.argv.(1))
        !files
