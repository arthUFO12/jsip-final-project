(* Tests for {!Ticker_name}: the normalized spelling of tickers inside
   programs and the map back to real slugs. *)

open! Core
open Types
open Parser

let%expect_test "normalize lowercases and maps every hyphen to an underscore"
  =
  List.iter
    [ "KXELONMARS-99"; "KXALBUMRELEASEDATETRIPPIE--26OCT01"; "save-act" ]
    ~f:(fun raw -> print_endline (Ticker_name.normalize raw));
  [%expect
    {|
    kxelonmars_99
    kxalbumreleasedatetrippie__26oct01
    save_act
    |}]
;;

let%expect_test "map values are the original slugs" =
  let slugs = List.map [ "KXA--1"; "KXB-2" ] ~f:Slug.of_string in
  let map = Ticker_name.build_map slugs |> Or_error.ok_exn in
  print_s [%sexp (map : Slug.t String.Map.t)];
  [%expect {| ((kxa__1 KXA--1) (kxb_2 KXB-2)) |}]
;;

let%expect_test "slugs that normalize identically are rejected" =
  let slugs = List.map [ "KX-A"; "kx_a" ] ~f:Slug.of_string in
  print_s [%sexp (Ticker_name.build_map slugs : _ Or_error.t)];
  [%expect
    {|
    (Error
     ("two market tickers normalize to the same name" (name kx_a) (slug kx_a)
      (clashes_with KX-A)))
    |}]
;;

let%expect_test "a ticker spelling a keyword is rejected" =
  print_s
    [%sexp (Ticker_name.build_map [ Slug.of_string "PRICE" ] : _ Or_error.t)];
  [%expect
    {|
    (Error
     ("market ticker collides with a language keyword" (slug PRICE)
      (keyword price)))
    |}]
;;

let%expect_test "a ticker that cannot lex as one word is rejected" =
  print_s
    [%sexp (Ticker_name.build_map [ Slug.of_string "99-UP" ] : _ Or_error.t)];
  [%expect
    {|
    (Error
     ("market ticker cannot be written as a program name" (slug 99-UP)
      (normalized 99_up)))
    |}]
;;
