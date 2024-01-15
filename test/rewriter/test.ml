open Claml

module F = Format

let () =
  let _ = Clang.initialize () in
  let ast =
    Clang.TranslationUnit.parse_file_internal [| Sys.argv.(1) |]
  in
  let tu = Clang.TranslationUnit.get_translation_unit ast in
  let rw = Clang.TranslationUnit.get_rewriter ast in

  let rec insert_comment decl =
    match decl with
    | None -> ()
    | Some decl ->
    (match Clang.Decl.get_kind decl with
    | Clang.DeclKind.FunctionDecl  ->
      let _ = Clang.Rewriter.insert_before_decl decl "/* inserted */\n" rw in
      ()
    | _ ->
      let next = Clang.TranslationUnit.decls_succ decl in
      insert_comment next) in
  
  insert_comment (Clang.TranslationUnit.decls_begin tu);

  Clang.Rewriter.emit_string rw
  |> print_endline;
  
  ()
  
