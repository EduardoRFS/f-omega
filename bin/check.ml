open Fomega

let max_line_size = 1024 * 1024

let write_type buf term =
  Eio.Buf_write.string buf (Format.asprintf "%a" Ttree.pp_type term)

let infer_expr_string string =
  match Lexer.from_string Parser.expr_opt string with
  | Some expr -> Some (Typer.infer_expr expr)
  | None -> None

let check buf line =
  match infer_expr_string line with
  | Some typ_ ->
      let typ_ = Typer.normalize typ_ in
      write_type buf typ_
  | None -> ()

let main () =
  Eio_main.run @@ fun env ->
  let stdin = Eio.Stdenv.stdin env in
  let stdout = Eio.Stdenv.stdout env in
  let read_buf = Eio.Buf_read.of_flow ~max_size:max_line_size stdin in
  Eio.Buf_write.with_flow stdout @@ fun write_buf ->
  let line = Eio.Buf_read.take_all read_buf in
  check write_buf line

let () = main ()
