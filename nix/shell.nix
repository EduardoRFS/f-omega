{ pkgs, fomega }:

with pkgs; with ocamlPackages; mkShell {
  inputsFrom = [ fomega ];
  packages = [
    # Make developer life easier
    # formatters
    nixfmt
    ocamlformat

    # OCaml developer tooling
    ocaml
    dune_3
    ocaml-lsp
    ocamlformat-rpc
  ];
}
