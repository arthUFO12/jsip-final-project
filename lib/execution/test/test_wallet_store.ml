open! Core
open! Async
open Expect_test_helpers_core

(* Exercises the encrypted wallet file end to end: save → status → unlock
   with the right and wrong passphrase → forget. Uses the same throwaway RSA
   key as {!Test_kalshi_live} — it has never touched a real account. *)

let with_wallet_path f =
  let%bind dir = Unix.mkdtemp "/tmp/wallet-store-test" in
  let path = dir ^/ "arbiter-wallet" in
  let%bind () = f ~path in
  let%bind () =
    Monitor.try_with (fun () -> Unix.unlink path)
    >>| (ignore : (unit, exn) Result.t -> unit)
  in
  Unix.rmdir dir
;;

let save_test_wallet ~path ~production =
  Execution.Wallet_store.save
    ~path
    ~key_id:"cc2a8db2-0000-0000-0000-e69f72455e86"
    ~private_key_pem:Test_kalshi_live.test_key_pem
    ~passphrase:"correct horse battery staple"
    ~production
    ()
;;

let%expect_test "save, status, unlock round-trip" =
  with_wallet_path (fun ~path ->
    let%bind () = save_test_wallet ~path ~production:false >>| ok_exn in
    let%bind status = Execution.Wallet_store.status ~path () in
    print_s [%sexp (status : Execution.Wallet_store.Status.t)];
    [%expect {|
      (Connected
        (key_hint   cc2a..5e86)
        (production false))
      |}];
    let%bind credentials =
      Execution.Wallet_store.unlock
        ~path
        ~passphrase:"correct horse battery staple"
        ()
      >>| ok_exn
    in
    (* The demo host was recorded at save time and must come back. *)
    print_endline (Execution.Kalshi_live.Credentials.host credentials);
    [%expect {| demo-api.kalshi.co |}];
    return ())
;;

let%expect_test "wrong passphrase fails the tag check, loudly and typed" =
  with_wallet_path (fun ~path ->
    let%bind () = save_test_wallet ~path ~production:false >>| ok_exn in
    let%bind result =
      Execution.Wallet_store.unlock ~path ~passphrase:"not the passphrase" ()
    in
    print_s [%sexp (result : (_ : Execution.Kalshi_live.Credentials.t) Or_error.t)];
    [%expect {| (Error "wrong passphrase (or the wallet file was modified)") |}];
    return ())
;;

let%expect_test "weak passphrase and junk PEM are refused before disk" =
  with_wallet_path (fun ~path ->
    let%bind short =
      Execution.Wallet_store.save
        ~path
        ~key_id:"k"
        ~private_key_pem:Test_kalshi_live.test_key_pem
        ~passphrase:"short"
        ~production:false
        ()
    in
    print_s [%sexp (short : unit Or_error.t)];
    [%expect {| (Error ("passphrase too short" (minimum 8))) |}];
    let%bind junk =
      Execution.Wallet_store.save
        ~path
        ~key_id:"k"
        ~private_key_pem:"not a pem"
        ~passphrase:"long enough though"
        ~production:false
        ()
    in
    print_s [%sexp (junk : unit Or_error.t)];
    [%expect
      {| (Error ("could not decode kalshi private key PEM" (message "No private key"))) |}];
    (* Neither refusal should have created the file. *)
    let%bind status = Execution.Wallet_store.status ~path () in
    print_s [%sexp (status : Execution.Wallet_store.Status.t)];
    [%expect {| Not_connected |}];
    return ())
;;

let%expect_test "forget deletes; forgetting nothing is fine" =
  with_wallet_path (fun ~path ->
    let%bind () = save_test_wallet ~path ~production:true >>| ok_exn in
    let%bind () = Execution.Wallet_store.forget ~path () >>| ok_exn in
    let%bind status = Execution.Wallet_store.status ~path () in
    print_s [%sexp (status : Execution.Wallet_store.Status.t)];
    [%expect {| Not_connected |}];
    let%bind () = Execution.Wallet_store.forget ~path () >>| ok_exn in
    [%expect {| |}];
    return ())
;;

let%expect_test "group/world-readable wallet is refused" =
  with_wallet_path (fun ~path ->
    let%bind () = save_test_wallet ~path ~production:false >>| ok_exn in
    let%bind () = Unix.chmod path ~perm:0o644 in
    let%bind result =
      Execution.Wallet_store.unlock
        ~path
        ~passphrase:"correct horse battery staple"
        ()
    in
    (match result with
     | Ok (_ : Execution.Kalshi_live.Credentials.t) ->
       print_endline "BUG: unlocked a world-readable wallet"
     | Error error ->
       (* The path inside is a temp dir; print only the stable part. *)
       let message = Error.to_string_hum error in
       print_endline
         (match
            String.is_substring message ~substring:"chmod 600"
          with
          | true -> "refused: chmod 600 advice given"
          | false -> message));
    [%expect {| refused: chmod 600 advice given |}];
    return ())
;;
