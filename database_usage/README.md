# Caqti Usage

## Setup
To create the database, it's as simple as creating a file in your current directory. The standard is to use the `.db` extension. To create a database named test.db:
```bash
touch test.db
```

It is important to know the absolute path of this file for future use. To get it use:
```bash
realpath test.db
```

Before running caqti with sqlite, it is important to have the following dependenceis in the dune file for the module
```ocaml
(executable
 (name main)
 (libraries caqti caqti-async caqti-driver-sqlite3 async))
```

## Code
To define sql queries, you will need to utilize the `Caqti_type` and `Caqti_request` libraries. 

### Types
Let `T` = `Caqti_type`. `T` has many types for the base types of OCaml, `T.int64`, `T.string`, etc. You can define a type for a tuple using this syntax: `T.(tup3 int64 string string)`. Note the `tup#` changes depending on the size of the tuple.

To use a record type in Caqti queries, you must define a way to serialize and deserialize it. This is the syntax:
```ocaml
type custom = {
  value : int
}
let custom_type : custom Caqti_type.t =
  let representation = T.int64 in
  let encode { value } = Ok value in
  let decode value = Ok { value } in
  T.custom ~encode ~decode representation
```

Then use the `custom_type` the same way you would any other type.

### Creating and inserting
`Caqti_request.exec` takes a `Caqti_type` and a sequal query wrapped in {||} as an argument, and returns a command that takes the OCaml types specified by the `Caqti_type` as arguments and returns nothing. An exmaple of a create table command is below

```ocaml
let create_table =
  Caqti_request.exec Caqti_type.unit
    {|
    CREATE TABLE IF NOT EXISTS users (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL,
      email TEXT NOT NULL UNIQUE
    )
    |}
```

Notice that we only have to define the input type.

### Finding
Use `Caqti_request.find` to create a command that selects rows from a table. This function takes both input and output `Caqti_type`'s as arguments along with a query.

```ocaml
let get_item_query =
  R.find T.int64 T.string "SELECT title FROM items WHERE id = ?"
```

### Actually Executing a Command

To run a command, you need a pool connection. To run the `get_item_query` from earlier, you would call
```ocaml
let execute_get_item (module Connection : Caqti_async.CONNECTION) id =
  C.exec get_item_query id

```

To initialize the database, this code would be ran:

```ocaml
open Core
open Async

module T = Caqti_type
module R = Caqti_request

(* -------------------------------------------------------------------------- *)
(* 1. File-Global Context                                                     *)
(* -------------------------------------------------------------------------- *)
(* Holds None at startup; updated to Some pool once initialized *)
let global_pool : Caqti_async.Pool.t option ref = ref None

(* Internal helper to ensure the pool exists before running a query *)
let with_pool f =
  match !global_pool with
  | None -> 
      (* Fail safely into the Deferred.Result monad *)
      Deferred.Result.fail (Caqti_error.connect_failed "Database pool is not initialized")
  | Some pool -> 
      Caqti_async.Pool.use f pool

(* -------------------------------------------------------------------------- *)
(* 2. Exported Functions                                                      *)
(* -------------------------------------------------------------------------- *)

(* Setup function called exactly once at application startup *)
let init_database uri_string =
  let db_uri = Uri.of_string uri_string in
  match Caqti_async.connect_pool db_uri with
  | Error err -> Error err
  | Ok pool ->
      global_pool := Some pool;
      Ok ()

(* Query function exported to the rest of your application *)
let add_user name email =
  let query = R.exec T.(tup2 string string) "INSERT INTO users (name, email) VALUES (?, ?)" in
  with_pool (fun (module C : Caqti_async.CONNECTION) ->
    C.exec query (name, email))

(* Another query function exported to the rest of your application *)
let get_user_email id =
  let query = R.find T.int64 T.string "SELECT email FROM users WHERE id = ?" in
  with_pool (fun (module C : Caqti_async.CONNECTION) ->
    C.find query id)
```