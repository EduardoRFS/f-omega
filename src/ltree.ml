type name = string

and expr =
  | LE_var of name
  | LE_t_lambda of name * kind option * expr
  | LE_t_apply of expr * typ_
  | LE_e_lambda of name * typ_ option * expr
  | LE_e_apply of expr * expr
  | LE_k_alias of name * kind * expr
  | LE_t_alias of name * typ_ * expr
  | LE_e_alias of name * expr * expr
  | LE_annot of expr * typ_

and typ_ =
  | LT_var of name
  | LT_forall of name * kind option * typ_
  | LT_arrow of typ_ * typ_
  | LT_lambda of name * kind option * typ_
  | LT_apply of typ_ * typ_
  | LT_annot of typ_ * kind

and kind = LK_var of name | LK_type | LK_arrow of kind * kind

type 'a ty = Int : int ty | String : string ty
