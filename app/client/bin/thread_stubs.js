// Some dependency links OCaml's threads library, whose init primitive has
// no js_of_ocaml implementation. JavaScript is single-threaded, so
// initializing thread support is a no-op — without this stub the release
// bundle raises at startup and the app never mounts.

//Provides: caml_thread_initialize
function caml_thread_initialize(unit) {
  return 0;
}
