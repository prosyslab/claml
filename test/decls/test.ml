open Claml
open Clang

let input = ref ""

let _ =
  Arg.parse []
    (fun s -> input := s)
    (Format.sprintf "Usage: %s <file>" Sys.argv.(0));
  let tu = TranslationUnit.parse_file [| !input |] in
  TranslationUnit.iter_decls
    (fun decl ->
      let loc = Decl.get_source_location decl in
      match loc with
      | Some loc ->
          if loc.filename <> !input then ()
          else (
            F.printf "%s:%d:%d @ %s: %!" loc.filename loc.line loc.column
              (Decl.get_kind_name decl);
            F.printf "%s@." (NamedDecl.get_name decl))
      | None -> F.printf "None@.")
    tu
