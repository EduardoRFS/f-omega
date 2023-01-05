open Ltree
open Ttree

exception Unbound_var of { name : name }
exception Variable_is_value of { name : name }
exception Variable_is_type of { name : name }
exception Variable_is_kind of { name : name }
exception Var_clash
exception Kind_clash of { received : kind; expected : kind }
exception Type_clash of { received : typ_; expected : typ_ }
exception Type_not_a_forall of { typ_ : typ_ }
exception Type_not_an_arrow of { typ_ : typ_ }
exception Type_not_a_constructor of { typ_ : typ_ }
exception Type_annotation_required

let () =
  Printexc.register_printer (function
    | Type_clash { expected; received } ->
        Some
          (Format.asprintf "expected: %a, received: %a" Ttree.pp_type expected
             Ttree.pp_type received)
    | _ -> None)

let rec subst ~from ~to_ typ_ =
  let (Type { desc; kind }) = typ_ in
  let desc =
    match desc with
    | T_var var -> (
        match Var.equal var from with true -> to_ | false -> T_var var)
    | T_forall (var, kind, body) ->
        let body =
          match Var.equal var from with
          | true -> body
          | false -> subst ~from ~to_ body
        in
        T_forall (var, kind, body)
    | T_arrow (param, return) ->
        let param = subst ~from ~to_ param in
        let return = subst ~from ~to_ return in
        T_arrow (param, return)
    | T_lambda (var, kind, body) ->
        let body =
          match Var.equal var from with
          | true -> body
          | false -> subst ~from ~to_ body
        in
        T_lambda (var, kind, body)
    | T_apply (lambda, arg) ->
        let lambda = subst ~from ~to_ lambda in
        let arg = subst ~from ~to_ arg in
        T_apply (lambda, arg)
  in
  Type { desc; kind }

let rec normalize typ_ =
  let (Type { desc; kind }) = typ_ in
  let desc =
    match desc with
    | T_var var -> T_var var
    | T_forall (var, kind, body) ->
        let body = normalize body in
        T_forall (var, kind, body)
    | T_arrow (param, return) ->
        let param = normalize param in
        let return = normalize return in
        T_arrow (param, return)
    | T_lambda (var, kind, body) ->
        let body = normalize body in
        T_lambda (var, kind, body)
    | T_apply (lambda, arg) -> (
        let lambda = normalize lambda in
        let arg = normalize arg in
        match
          let (Type { desc = lambda_desc; kind = _ }) = normalize lambda in
          lambda_desc
        with
        | T_lambda (var, _kind, body) ->
            (* TODO: this is really bad *)
            let (Type { desc = arg; kind = _ }) = arg in
            let (Type { desc = body; kind = body_kind }) =
              subst ~from:var ~to_:arg body
            in
            let body = normalize @@ Type { desc = body; kind = body_kind } in
            let (Type { desc = body; kind = _ }) = body in
            body
        | _ -> T_apply (lambda, arg))
  in
  Type { desc; kind }

let rec equal_kind ~received ~expected =
  match (received, expected) with
  | K_type, K_type -> ()
  | ( K_arrow (received_param, received_return),
      K_arrow (expected_param, expected_return) ) ->
      equal_kind ~received:received_param ~expected:expected_param;
      equal_kind ~received:received_return ~expected:expected_return
  | received, expected -> raise (Kind_clash { received; expected })

let rec equal ~received ~expected =
  let (Type { desc = received_desc; kind = received_kind }) = received in
  let (Type { desc = expected_desc; kind = expected_kind }) = expected in
  equal_kind ~received:received_kind ~expected:expected_kind;
  match (received_desc, expected_desc) with
  | T_var received, T_var expected -> (
      match received = expected with true -> () | false -> raise Var_clash)
  | ( T_forall (received_var, received_kind, received_body),
      T_forall (expected_var, expected_kind, expected_body) )
  | ( T_lambda (received_var, received_kind, received_body),
      T_lambda (expected_var, expected_kind, expected_body) ) ->
      equal_kind ~received:received_kind ~expected:expected_kind;
      let received_body =
        let to_ = T_var expected_var in
        subst ~from:received_var ~to_ received_body
      in
      equal ~received:received_body ~expected:expected_body
  | ( T_apply (received_lambda, received_arg),
      T_apply (expected_lambda, expected_arg) ) ->
      equal ~received:received_lambda ~expected:expected_lambda;
      equal ~received:received_arg ~expected:expected_arg
  | ( T_arrow (received_param, expected_body),
      T_arrow (expected_param, received_body) ) ->
      equal ~received:received_param ~expected:expected_param;
      equal ~received:received_body ~expected:expected_body
  | _, _ -> raise (Type_clash { received; expected })

let equal ~received ~expected =
  let received = normalize received in
  let expected = normalize expected in
  equal ~received ~expected

module Env : sig
  type 'a env

  val run : (unit -> 'a env) -> 'a

  module Syntax : sig
    val return : 'a -> 'a env
    val ( let* ) : 'a env -> ('a -> 'b env) -> 'b env
    val ( let+ ) : 'a env -> ('a -> 'b) -> 'b env
  end

  (* kind *)
  val find_kind : name:name -> Ltree.kind env
  val with_kind_alias : name:name -> Ltree.kind -> (unit -> 'a env) -> 'a env

  (* typ_ *)
  val find_type : name:name -> (Var.t * kind, Ltree.typ_) Either.t env
  val with_type : var:Var.t -> kind:kind -> (unit -> 'a env) -> 'a env
  val with_type_alias : name:name -> Ltree.typ_ -> (unit -> 'a env) -> 'a env

  (* expr *)
  val find_value : name:name -> (typ_, Ltree.expr) Either.t env
  val with_value : name:name -> typ_ -> (unit -> 'a env) -> 'a env
  val with_value_alias : name:name -> Ltree.expr -> (unit -> 'a env) -> 'a env
end = struct
  module Name_map = Map.Make (String)

  type var_desc =
    | Value of { typ_ : typ_ }
    | Value_alias of { alias : Ltree.expr }
    | Type of { var : Var.t; kind : kind }
    | Type_alias of { alias : Ltree.typ_ }
    | Kind_alias of { alias : Ltree.kind }

  type 'a env = names:var_desc Name_map.t -> 'a

  let run f =
    let names = Name_map.empty in
    f () ~names

  module Syntax = struct
    let return v ~names:_ = v

    let ( let* ) v f ~names =
      let value = v ~names in
      f value ~names

    let ( let+ ) v f ~names =
      let value = v ~names in
      f value
  end

  let find_kind ~name ~names =
    match Name_map.find_opt name names with
    | Some (Value { typ_ = _ } | Value_alias { alias = _ }) ->
        raise (Variable_is_value { name })
    | Some (Type { var = _; kind = _ } | Type_alias { alias = _ }) ->
        raise (Variable_is_type { name })
    | Some (Kind_alias { alias }) -> alias
    | None -> raise (Unbound_var { name })

  let find_type ~name ~names =
    match Name_map.find_opt name names with
    | Some (Value { typ_ = _ } | Value_alias { alias = _ }) ->
        raise (Variable_is_value { name })
    | Some (Type { var; kind }) -> Either.Left (var, kind)
    | Some (Type_alias { alias }) -> Either.Right alias
    | Some (Kind_alias { alias = _ }) -> raise (Variable_is_kind { name })
    | None -> raise (Unbound_var { name })

  let find_value ~name ~names =
    match Name_map.find_opt name names with
    | Some (Value { typ_ }) -> Either.Left typ_
    | Some (Value_alias { alias }) -> Either.Right alias
    | Some (Type { var = _; kind = _ } | Type_alias { alias = _ }) ->
        raise (Variable_is_type { name })
    | Some (Kind_alias { alias = _ }) -> raise (Variable_is_kind { name })
    | None -> raise (Unbound_var { name })

  let with_var name desc ~names f =
    let names = Name_map.add name desc names in
    f () ~names

  let with_kind_alias ~name alias f ~names =
    with_var name (Kind_alias { alias }) ~names f

  let with_type ~var ~kind f ~names =
    let name = Var.name var in
    with_var name (Type { var; kind }) ~names f

  let with_type_alias ~name alias f ~names =
    with_var name (Type_alias { alias }) ~names f

  let with_value ~name typ_ f ~names = with_var name (Value { typ_ }) ~names f

  let with_value_alias ~name alias f ~names =
    with_var name (Value_alias { alias }) ~names f
end

open Env.Syntax

let rec transl_kind kind =
  match kind with
  | LK_var name ->
      let* kind = Env.find_kind ~name in
      transl_kind kind
  | LK_type -> return @@ K_type
  | LK_arrow (param, return) ->
      let* param = transl_kind param in
      let+ return = transl_kind return in
      K_arrow (param, return)

let type_ kind desc = Type { desc; kind }

let rec infer_type typ_ =
  match typ_ with
  | LT_var name -> (
      let* desc = Env.find_type ~name in
      match desc with
      | Left (var, kind) -> return @@ type_ kind @@ T_var var
      | Right alias -> infer_type alias)
  | LT_forall (name, Some annot, body) ->
      let var = Var.new_var ~name in
      let* kind = transl_kind annot in
      let+ body =
        Env.with_type ~var ~kind @@ fun () -> check_type body ~expected:K_type
      in
      type_ K_type @@ T_forall (var, kind, body)
  | LT_forall (name, None, body) ->
      (* TODO: this behaviour is not ideal *)
      infer_type (LT_forall (name, Some LK_type, body))
  | LT_arrow (param, return) ->
      let* param = check_type param ~expected:K_type in
      let+ return = check_type return ~expected:K_type in
      type_ K_type @@ T_arrow (param, return)
  | LT_lambda (name, Some annot, body) ->
      let var = Var.new_var ~name in
      let* kind = transl_kind annot in
      let+ body = Env.with_type ~var ~kind @@ fun () -> infer_type body in
      let (Type { desc = _; kind = body_kind }) = body in
      type_ (K_arrow (kind, body_kind)) @@ T_lambda (var, kind, body)
  | LT_lambda (name, None, body) ->
      (* TODO: this behaviour is not ideal *)
      infer_type (LT_lambda (name, Some LK_type, body))
  | LT_apply (lambda, arg) -> (
      let* lambda = infer_type lambda in
      let (Type { desc = _; kind }) = lambda in
      match kind with
      | K_type -> raise (Type_not_a_constructor { typ_ = lambda })
      | K_arrow (param, return) ->
          let+ arg = check_type arg ~expected:param in
          type_ return @@ T_apply (lambda, arg))
  | LT_annot (typ_, annot) ->
      let* annot = transl_kind annot in
      check_type typ_ ~expected:annot

and check_type typ_ ~expected =
  match (typ_, expected) with
  | LT_lambda (name, None, body), K_arrow (kind, return) ->
      let var = Var.new_var ~name in
      let+ body =
        Env.with_type ~var ~kind @@ fun () -> check_type body ~expected:return
      in
      let (Type { desc = _; kind = body_kind }) = body in
      type_ (K_arrow (kind, body_kind)) @@ T_lambda (var, kind, body)
  | typ_, expected ->
      let+ received = infer_type typ_ in
      let () =
        let (Type { desc = _; kind = received }) = received in
        equal_kind ~received ~expected
      in
      received

let rec infer_expr expr =
  match expr with
  | LE_var name -> (
      let* desc = Env.find_value ~name in
      match desc with
      | Left typ_ -> return typ_
      | Right alias -> infer_expr alias)
  | LE_t_lambda (name, Some annot, body) ->
      let var = Var.new_var ~name in
      let* kind = transl_kind annot in
      let+ body = Env.with_type ~var ~kind @@ fun () -> infer_expr body in
      type_ K_type @@ T_forall (var, kind, body)
  | LE_t_lambda (name, None, body) ->
      (* TODO: this behaviour is not ideal *)
      infer_expr (LE_t_lambda (name, Some LK_type, body))
  | LE_t_apply (lambda, arg) -> (
      let* forall = infer_expr lambda in
      match
        let (Type { desc; kind = _ }) = normalize forall in
        desc
      with
      | T_forall (var, kind, body) ->
          let+ arg = check_type arg ~expected:kind in
          let (Type { desc = arg; kind = _ }) = arg in
          subst ~from:var ~to_:arg body
      | _ -> raise (Type_not_a_forall { typ_ = forall }))
  | LE_e_lambda (name, Some param, body) ->
      let* param = check_type param ~expected:K_type in
      let+ return = Env.with_value ~name param @@ fun () -> infer_expr body in
      type_ K_type @@ T_arrow (param, return)
  | LE_e_lambda (_var, None, _body) -> raise Type_annotation_required
  | LE_e_apply (lambda, arg) -> (
      let* arrow = infer_expr lambda in
      match
        let (Type { desc; kind = _ }) = normalize arrow in
        desc
      with
      | T_arrow (param, return) ->
          let+ () = check_expr arg ~expected:param in
          return
      | _ -> raise (Type_not_an_arrow { typ_ = arrow }))
  | LE_k_alias (name, kind, body) ->
      Env.with_kind_alias ~name kind @@ fun () -> infer_expr body
  | LE_t_alias (name, typ_, body) ->
      Env.with_type_alias ~name typ_ @@ fun () -> infer_expr body
  | LE_e_alias (name, value, body) ->
      Env.with_value_alias ~name value @@ fun () -> infer_expr body
  | LE_annot (value, annot) ->
      let* annot = check_type annot ~expected:K_type in
      let+ () = check_expr value ~expected:annot in
      annot

and check_expr expr ~expected =
  match
    (* TODO: this is not ideal if aliasing is considered
        it will expand when not needed, probably head expansion *)
    let (Type { desc = expected_desc; kind = _ }) = normalize expected in
    (expr, expected_desc)
  with
  | ( LE_t_lambda (name, received_kind, body),
      T_forall (expected_var, expected_kind, expected_return) ) ->
      let var = Var.new_var ~name in
      let* kind =
        match received_kind with
        | None -> return expected_kind
        | Some received_kind ->
            let+ received_kind = transl_kind received_kind in
            let () =
              equal_kind ~received:received_kind ~expected:expected_kind
            in
            expected_kind
      in
      let return = subst ~from:expected_var ~to_:(T_var var) expected_return in
      Env.with_type ~var ~kind @@ fun () -> check_expr body ~expected:return
  | ( LE_e_lambda (name, received_param, body),
      T_arrow (expected_param, expected_return) ) ->
      let* param =
        match received_param with
        | None -> return expected_param
        | Some received_param ->
            let+ received_param = check_type received_param ~expected:K_type in
            let () = equal ~received:received_param ~expected:expected_param in
            expected_param
      in
      Env.with_value ~name param @@ fun () ->
      check_expr body ~expected:expected_return
  | expr, _ ->
      let+ received = infer_expr expr in
      equal ~received ~expected

let infer_type typ_ = Env.run @@ fun () -> infer_type typ_
let infer_expr expr = Env.run @@ fun () -> infer_expr expr
