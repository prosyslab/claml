let _ =
  let tu = Clang.TranslationUnit.parse_file [| Sys.argv.(1) |] in
  Format.printf "%a" Clang.TranslationUnit.pp tu
