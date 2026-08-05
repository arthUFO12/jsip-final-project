open! Core

let rng_seed_length = 32

let initialize =
  lazy
    (let seed =
       In_channel.with_file "/dev/urandom" ~f:(fun channel ->
         let buf = Bytes.create rng_seed_length in
         match
           In_channel.really_input channel ~buf ~pos:0 ~len:rng_seed_length
         with
         | Some () -> Bytes.to_string buf
         | None -> raise_s [%message "unexpected end of /dev/urandom"])
     in
     Mirage_crypto_rng.set_default_generator
       (Mirage_crypto_rng.create
          ~seed:(Cstruct.of_string seed)
          (module Mirage_crypto_rng.Fortuna)))
;;

let ensure () = force initialize
