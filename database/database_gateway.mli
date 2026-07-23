open! Core

(** Connect to the database and store the connection pool globally.
    Must be called before any other database operation. *)
val init_database : unit -> (unit, Caqti_error.t) result
