%{
open Ltree

(* TODO: better naming *)
let wrap_expr_lambda (params, return) body =
  let body =
    match return with
    | Some annot -> LE_annot (body, annot)
    | None -> body in
  List.fold_right (fun param body ->
    match param with
    | `Type (var, kind) -> LE_t_lambda (var, kind, body)
    | `Value (var, typ_) -> LE_e_lambda (var, typ_, body)
  ) params body
let wrap_type_lambda (params, return) body =
  let body =
    match return with
    | Some annot -> LT_annot (body, annot)
    | None -> body in
  let wrap (var, annot) body = LT_lambda (var, annot, body) in
  List.fold_right wrap params body
%}
%token <string> VAR (* x *)
%token FUN (* fun *)
%token COLON (* : *) 
%token DOT (* . *)
%token ARROW (* -> *)
%token ALIAS (* === *)
%token LET (* let *)
%token TYPE (* type *)
%token KIND (* kind *)
%token FORALL (* forall *)
%token IN (* in *)
%token STAR (* * *)
%token LEFT_PARENS (* ( *)
%token RIGHT_PARENS (* ) *)
%token EOF

%start <Ltree.expr option> expr_opt
%start <Ltree.typ_ option> type_opt
%start <Ltree.kind option> kind_opt

%%

let expr_opt :=
  | EOF;
    { None }
  | expr = expr; EOF;
    { Some expr }
let type_opt :=
  | EOF;
    { None }
  | typ_ = typ_; EOF;
    { Some typ_ }
let kind_opt :=
  | EOF;
    { None }
  | kind = kind; EOF;
    { Some kind }

let expr == expr_annot

let expr_annot ==
  | value = expr_funct; COLON; annot = typ_;
    { LE_annot (value, annot) }
  | expr_bind_in

let expr_bind_in :=
  | KIND; var = VAR; ALIAS; value = kind; IN; body = expr_bind_in;
    { LE_k_alias (var, value, body) }
  | TYPE; var = VAR; (params, annot) = type_bind_params;
    ALIAS; value = typ_; IN; body = expr_bind_in;
    { let value = wrap_type_lambda (params, annot) value in
      LE_t_alias (var, value, body) }
  | LET; var = VAR; (params, annot) = expr_bind_params;
    ALIAS; value = expr_bind_in; IN; body = expr_bind_in;
    { let value = wrap_expr_lambda (params, annot) value in
      LE_e_alias (var, value, body) }
  | expr_funct

let type_bind_params ==
  | params = list(type_param);
    { (params, None) }
  | params = list(type_param); COLON; return = kind;
    { (params, Some return) }
let expr_bind_params ==
  | params = list(expr_param);
    { (params, None) }
  | params = list(expr_param); COLON; return = typ_;
    { (params, Some return) }

let expr_funct :=
  | FUN; (params, return) = expr_funct_params; ARROW; body = expr_funct;
    { wrap_expr_lambda (params, return) body }
  | expr_apply

let expr_funct_params ==
  | params = nonempty_list(expr_param);
    { (params, None) }
  | params = nonempty_list(expr_param); COLON; return = type_apply;
    { (params, Some return) }

let expr_param ==
  | var = VAR;
    { `Value (var, None) }
  | LEFT_PARENS; var = VAR; COLON; typ_ = typ_; RIGHT_PARENS;
    { `Value (var, Some typ_) }
  | LEFT_PARENS; TYPE; var = VAR; RIGHT_PARENS;
    { `Type (var, None) }
  | LEFT_PARENS; TYPE; var = VAR; COLON; kind = kind; RIGHT_PARENS;
    { `Type (var, Some kind) }

let expr_apply :=
  | lambda = expr_apply; LEFT_PARENS; TYPE; arg = typ_; RIGHT_PARENS;
    { LE_t_apply (lambda, arg) }
  | lambda = expr_apply; arg = expr_atom;
    { LE_e_apply (lambda, arg) }
  | expr_atom

let expr_atom ==
  | var = VAR;
    { LE_var var }
  | LEFT_PARENS; expr = expr; RIGHT_PARENS;
    { expr }

let typ_ == type_funct

let type_funct :=
  | FORALL; vars = nonempty_list(type_param); DOT; body = type_funct;
    { let wrap (var, annot) body = LT_forall (var, annot, body) in
      List.fold_right wrap vars body }
  | FUN; (params, annot) = type_funct_params; ARROW; body = type_funct;
    { wrap_type_lambda (params, annot) body }
  | left = type_apply; ARROW; right = type_funct;
    { LT_arrow (left, right) }
  | type_apply

let type_funct_params ==
  | params = nonempty_list(type_param);
    { (params, None) }
  | params = nonempty_list(type_param); COLON; return = kind_atom;
    { (params, Some return) }

let type_param ==
  | var = VAR;
    { (var, None) }
  | LEFT_PARENS; var = VAR; COLON; kind = kind; RIGHT_PARENS;
    { (var, Some kind) }

let type_apply ==
  (* TODO: this is a hack *)
  | lambda = type_atom; args = list(type_atom);
    { let wrap lambda arg = LT_apply (lambda, arg) in
      List.fold_left wrap lambda args }

let type_atom ==
  | var = VAR;
    { LT_var var }
  | LEFT_PARENS; typ_ = typ_; RIGHT_PARENS;
    { typ_ }

let kind == kind_funct

let kind_funct :=
  | left = kind_atom; ARROW; right = kind_funct;
    { LK_arrow (left, right) }
  | kind_atom

let kind_atom ==
  | var = VAR;
    { LK_var var }
  | STAR;
    { LK_type }
  | LEFT_PARENS; kind = kind; RIGHT_PARENS;
    { kind }
