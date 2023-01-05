module Var : sig
  type var
  type t = var [@@deriving show, eq, ord]

  val name : var -> Ltree.name
  val new_var : name:Ltree.name -> var
end = struct
  type var = { id : int; name : string }
  type t = var

  let pp fmt var =
    let { id = _; name } = var in
    Format.fprintf fmt "%s" name

  let show var = Format.asprintf "%a" pp var
  let equal a b = a.id = b.id
  let compare a b = compare a.id b.id
  let name a = a.name

  let new_var =
    let acc = Atomic.make 0 in
    fun ~name ->
      let id = Atomic.fetch_and_add acc 1 in
      { id; name }
end

type kind = K_type | K_arrow of kind * kind

type typ_ = Type of { desc : typ_desc; kind : kind }

and typ_desc =
  | T_var of Var.t
  | T_forall of Var.t * kind * typ_
  | T_arrow of typ_ * typ_
  | T_lambda of Var.t * kind * typ_
  | T_apply of typ_ * typ_

open Format

type kind_prec = KP_funct | KP_atom

let pp_kind_syntax ~pp_funct ~pp_atom fmt kind =
  match kind with
  | K_type -> fprintf fmt "*"
  | K_arrow (param, return) ->
      fprintf fmt "%a -> %a" pp_atom param pp_funct return

let rec pp_kind prec fmt kind =
  let pp_funct fmt kind = pp_kind KP_funct fmt kind in
  let pp_atom fmt kind = pp_kind KP_atom fmt kind in
  match (kind, prec) with
  | K_type, (KP_funct | KP_atom) | K_arrow _, KP_funct ->
      pp_kind_syntax ~pp_funct ~pp_atom fmt kind
  | K_arrow _, KP_atom -> fprintf fmt "(%a)" pp_funct kind

let pp_kind fmt kind = pp_kind KP_funct fmt kind

type type_prec = Type_funct | Type_apply | Type_atom

let pp_type_syntax ~pp_funct ~pp_apply ~pp_atom fmt typ_ =
  let (Type { desc; kind = _ }) = typ_ in
  match desc with
  | T_var var -> Var.pp fmt var
  | T_forall (var, kind, return) -> (
      match kind with
      | K_type -> fprintf fmt "forall %a. %a" Var.pp var pp_funct return
      | K_arrow _ ->
          fprintf fmt "forall (%a : %a). %a" Var.pp var pp_kind kind pp_funct
            return)
  | T_arrow (param, return) ->
      fprintf fmt "%a -> %a" pp_atom param pp_funct return
  | T_lambda (var, kind, body) -> (
      match kind with
      | K_type -> fprintf fmt "fun %a -> %a" Var.pp var pp_funct body
      | K_arrow _ ->
          fprintf fmt "fun (%a : %a) -> %a" Var.pp var pp_kind kind pp_funct
            body)
  | T_apply (lambda, arg) -> fprintf fmt "%a %a" pp_apply lambda pp_atom arg

let rec pp_type prec fmt typ_ =
  let pp_funct fmt typ_ = pp_type Type_funct fmt typ_ in
  let pp_apply fmt typ_ = pp_type Type_apply fmt typ_ in
  let pp_atom fmt typ_ = pp_type Type_atom fmt typ_ in
  let (Type { desc; kind = _ }) = typ_ in
  match (desc, prec) with
  | T_var _, (Type_funct | Type_apply | Type_atom)
  | T_apply _, (Type_funct | Type_apply)
  | (T_forall _ | T_arrow _ | T_lambda _), Type_funct ->
      pp_type_syntax ~pp_funct ~pp_apply ~pp_atom fmt typ_
  | (T_forall _ | T_arrow _ | T_lambda _), (Type_apply | Type_atom)
  | T_apply _, Type_atom ->
      fprintf fmt "(%a)" pp_funct typ_

let pp_type fmt typ_ = pp_type Type_funct fmt typ_
