open Fomega

let () = Printexc.record_backtrace true
let debug = false
let max_line_size = 1024 * 1024
let write_welcome buf = Eio.Buf_write.string buf "Welcome to Fω"
let write_newline buf = Eio.Buf_write.char buf '\n'
let write_pending buf = Eio.Buf_write.string buf "> "

let write_exception buf exn =
  match debug with
  | true -> Eio.Buf_write.string buf (Printexc.get_backtrace ())
  | false -> Eio.Buf_write.string buf (Printexc.to_string exn)

let write_type buf term =
  Eio.Buf_write.string buf (Format.asprintf "%a" Ttree.pp_type term)

let infer_expr_string string =
  match Lexer.from_string Parser.expr_opt string with
  | Some expr -> Some (Typer.infer_expr expr)
  | None -> None

let check buf line =
  match infer_expr_string line with
  | Some typ_ -> write_type buf typ_
  | None -> ()

let run buf line = try check buf line with exn -> write_exception buf exn

let command buf line =
  let () =
    run buf line;
    write_newline buf
  in
  write_pending buf

let repl read_buf write_buf =
  write_welcome write_buf;
  write_newline write_buf;
  write_pending write_buf;
  let lines = Eio.Buf_read.lines read_buf in
  Seq.iter (fun line -> command write_buf line) lines

let main () =
  Eio_main.run @@ fun env ->
  let stdin = Eio.Stdenv.stdin env in
  let stdout = Eio.Stdenv.stdout env in
  let read_buf = Eio.Buf_read.of_flow ~max_size:max_line_size stdin in
  Eio.Buf_write.with_flow stdout @@ fun write_buf -> repl read_buf write_buf

let () = main ()
