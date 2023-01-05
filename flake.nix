{
  description = "Nix Flake";

  inputs = {
    nixpkgs.url = "github:anmonteiro/nix-overlays";
    nix-filter.url = "github:numtide/nix-filter";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nix-filter, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = (nixpkgs.makePkgs {
        inherit system;
        extraOverlays = [
          (import ./nix/overlay.nix)
        ];
      }).extend (self: super: {
        ocamlPackages = super.ocaml-ng.ocamlPackages_5_0;
      }); in
      let fomega = pkgs.callPackage ./nix {
        inherit nix-filter;
        doCheck = true;
      }; in
      rec {
        packages = { inherit fomega; };
        devShell = import ./nix/shell.nix { inherit pkgs fomega; };
      });
}
