# WIP parser types

Unfinished modules (`action`, `ast_node`, `conditional_action`, `signal`)
parked here so they don't break `dune build`. This directory has no `dune`
stanza, so dune ignores it. When a module compiles cleanly, move its
`.ml`/`.mli` back into `lib/types/src/`.
