open! Core
open Types

type t =
  { ast_root : Ast_node.t
  ; action : Action.t
  }

let conditionally_perform_action (t : t) : unit =
  if Ast_node.evaluate_node t.ast_root then Action.perform_action t.action
;;
