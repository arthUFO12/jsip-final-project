open! Core
open! Async

module Status = struct
  type t =
    | Not_connected
    | Connected of
        { key_hint : string
        ; production : bool
        }
  [@@deriving sexp_of, equal]
end

(* NIST SP 800-132 floor is 1000; OWASP's 2024 recommendation for
   PBKDF2-HMAC-SHA256 is 600k. Local unlock latency (~1s) is acceptable for a
   once-per-session action. *)
let kdf_iterations = 600_000
let derived_key_length = 32l
let salt_length = 16
let nonce_length = 12
let minimum_passphrase_length = 8
let version = 1

module Gcm = Mirage_crypto.Cipher_block.AES.GCM

(* The whole file is one sexp of this record. The payload (key id + PEM) is
   inside the ciphertext; only the display hint and host choice are plain,
   and both are bound into the GCM tag as associated data so they cannot be
   tampered with independently. *)
module On_disk = struct
  type t =
    { version : int
    ; production : bool
    ; key_hint : string
    ; kdf_iterations : int
    ; salt_b64 : string
    ; nonce_b64 : string
    ; ciphertext_b64 : string
    }
  [@@deriving sexp]
end

module Payload = struct
  type t =
    { key_id : string
    ; private_key_pem : string
    }
  [@@deriving sexp]
end

let default_path () =
  let home = Option.value (Sys.getenv "HOME") ~default:"." in
  home ^/ ".kalshi" ^/ "arbiter-wallet"
;;

let key_hint key_id =
  match String.length key_id >= 8 with
  | true -> [%string "%{String.prefix key_id 4}..%{String.suffix key_id 4}"]
  | false -> "key"
;;

let adata ~production ~key_hint =
  Cstruct.of_string [%string "%{version#Int}|%{production#Bool}|%{key_hint}"]
;;

let derive_key ~passphrase ~salt =
  Pbkdf.pbkdf2
    ~prf:`SHA256
    ~password:(Cstruct.of_string passphrase)
    ~salt
    ~count:kdf_iterations
    ~dk_len:derived_key_length
;;

(* Same contract as the raw key file in [Kalshi_live.Credentials]: loose
   permission bits are refused, not warned about. *)
let check_permissions path =
  let open Deferred.Or_error.Let_syntax in
  let%bind stat =
    Monitor.try_with_or_error ~name:"stat wallet file" (fun () ->
      Unix.stat path)
  in
  let loose_bits = stat.perm land 0o077 in
  match loose_bits = 0 with
  | true -> return ()
  | false ->
    Deferred.Or_error.error_s
      [%message
        "refusing group/world-readable wallet file; run: chmod 600 <file>"
          (path : string)
          ~permissions:(sprintf "0o%o" stat.perm : string)]
;;

let save ~path ~key_id ~private_key_pem ~passphrase ~production () =
  let open Deferred.Or_error.Let_syntax in
  let%bind () =
    match String.length passphrase >= minimum_passphrase_length with
    | true -> return ()
    | false ->
      Deferred.Or_error.error_s
        [%message
          "passphrase too short" ~minimum:(minimum_passphrase_length : int)]
  in
  (* Prove the key signs for the chosen host before anything hits disk — a
     typo'd PEM should fail here, not at the first order. *)
  let host =
    match production with
    | true -> Kalshi_live.live_host
    | false -> Kalshi_live.demo_host
  in
  let%bind (_ : Kalshi_live.Credentials.t) =
    Deferred.return
      (Kalshi_live.Credentials.create ~host ~key_id ~private_key_pem ())
  in
  Entropy.ensure ();
  let salt = Mirage_crypto_rng.generate salt_length in
  let nonce = Mirage_crypto_rng.generate nonce_length in
  let key = Gcm.of_secret (derive_key ~passphrase ~salt) in
  let hint = key_hint key_id in
  let plaintext =
    Payload.sexp_of_t { key_id; private_key_pem }
    |> Sexp.to_string
    |> Cstruct.of_string
  in
  let ciphertext =
    Gcm.authenticate_encrypt
      ~key
      ~nonce
      ~adata:(adata ~production ~key_hint:hint)
      plaintext
  in
  let on_disk =
    { On_disk.version
    ; production
    ; key_hint = hint
    ; kdf_iterations
    ; salt_b64 = Base64.encode_exn (Cstruct.to_string salt)
    ; nonce_b64 = Base64.encode_exn (Cstruct.to_string nonce)
    ; ciphertext_b64 = Base64.encode_exn (Cstruct.to_string ciphertext)
    }
  in
  Monitor.try_with_or_error ~name:"write wallet file" (fun () ->
    let%bind.Deferred () =
      Unix.mkdir ~p:() ~perm:0o700 (Filename.dirname path)
    in
    let%bind.Deferred () =
      Writer.save
        path
        ~contents:(Sexp.to_string_hum (On_disk.sexp_of_t on_disk))
    in
    Unix.chmod path ~perm:0o600)
;;

let read_on_disk ~path =
  let open Deferred.Or_error.Let_syntax in
  let%bind () = check_permissions path in
  let%bind contents =
    Monitor.try_with_or_error ~name:"read wallet file" (fun () ->
      Reader.file_contents path)
  in
  (* Human-adjacent input off disk: parse defensively. *)
  match
    Or_error.try_with (fun () -> On_disk.t_of_sexp (Sexp.of_string contents))
  with
  | Ok on_disk -> return on_disk
  | Error error ->
    Deferred.Or_error.error_s
      [%message
        "wallet file did not parse" (path : string) (error : Error.t)]
;;

let status ~path () =
  match%bind Sys.file_exists_exn path with
  | false -> return Status.Not_connected
  | true ->
    (match%map read_on_disk ~path with
     | Error (_ : Error.t) -> Status.Not_connected
     | Ok { On_disk.key_hint; production; _ } ->
       Status.Connected { key_hint; production })
;;

let unlock ~path ~passphrase () =
  let open Deferred.Or_error.Let_syntax in
  let%bind () =
    match%bind.Deferred Sys.file_exists_exn path with
    | true -> return ()
    | false ->
      Deferred.Or_error.error_s
        [%message "no wallet is connected" (path : string)]
  in
  let%bind on_disk = read_on_disk ~path in
  let%bind decoded =
    Deferred.return
      (Or_error.try_with (fun () ->
         ( Cstruct.of_string (Base64.decode_exn on_disk.salt_b64)
         , Cstruct.of_string (Base64.decode_exn on_disk.nonce_b64)
         , Cstruct.of_string (Base64.decode_exn on_disk.ciphertext_b64) )))
  in
  let salt, nonce, ciphertext = decoded in
  let key = Gcm.of_secret (derive_key ~passphrase ~salt) in
  match
    Gcm.authenticate_decrypt
      ~key
      ~nonce
      ~adata:
        (adata ~production:on_disk.production ~key_hint:on_disk.key_hint)
      ciphertext
  with
  | None ->
    Deferred.Or_error.error_string
      "wrong passphrase (or the wallet file was modified)"
  | Some plaintext ->
    let%bind payload =
      Deferred.return
        (Or_error.try_with (fun () ->
           Payload.t_of_sexp (Sexp.of_string (Cstruct.to_string plaintext))))
    in
    let host =
      match on_disk.production with
      | true -> Kalshi_live.live_host
      | false -> Kalshi_live.demo_host
    in
    Deferred.return
      (Kalshi_live.Credentials.create
         ~host
         ~key_id:payload.key_id
         ~private_key_pem:payload.private_key_pem
         ())
;;

let forget ~path () =
  match%bind Sys.file_exists_exn path with
  | false -> Deferred.Or_error.return ()
  | true ->
    Monitor.try_with_or_error ~name:"delete wallet file" (fun () ->
      Unix.unlink path)
;;
