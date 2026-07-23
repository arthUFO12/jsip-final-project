(lang dune 3.0)

(generate_opam_files true)

(maintainers "Nigel Tatem & Arthur Ufongene")

(authors "Nigel Tatem & Arthur Ufongene")

(homepage "https://github.com/arthUFO12/jsip-final-project")

(bug_reports "https://github.com/issues")

(license MIT)

(package
 (name arbiter)
 (synopsis "Prediction-market arbitrage scanner and strategy backtester")
 (depends
  (ocaml
   (>= 5.2.0))
  dune
  ocaml-lsp-server
  ocamlformat
  (odoc :with-doc)
  async
  async_log
  async_rpc_kernel
  async_rpc_websocket
  bonsai
  bonsai_term
  bonsai_term_components
  bonsai_web
  virtual_dom
  cohttp-async
  uri
  yojson
  postgresql
  core
  core_unix
  core_bench
  ppx_jane
  ppx_html
  js_of_ocaml-ppx
  (bin_prot :with-test)
  (notty-community :with-test)
  (expect_test_helpers_core :with-test)
  (expect_test_helpers_async :with-test)))
