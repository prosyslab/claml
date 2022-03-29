let _ =
  let tu = Claml.Clang.TranslationUnit.parse_file [| Sys.argv.(1) |] in
  Format.printf "%a" Claml.Clang.TranslationUnit.pp tu
