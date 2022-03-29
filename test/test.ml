let _ =
  if Sys.file_exists Sys.argv.(1) then
    let tu = Claml.Clang.TranslationUnit.parse_file [| Sys.argv.(1) |] in
    Format.printf "%a" Claml.Clang.TranslationUnit.pp tu
  else Printf.eprintf "%s not found\n" Sys.argv.(1)
