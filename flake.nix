{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
  inputs.opam-nix = {
    url = "github:tweag/opam-nix";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-utils.follows = "flake-utils";
    inputs.opam-repository.follows = "opam-repository";
    inputs.opam-overlays.follows = "opam-overlays";
    inputs.mirage-opam-overlays.follows = "mirage-opam-overlays";
  };
  inputs.opam-repository = {
    url = "github:ocaml/opam-repository";
    flake = false;
  };
  inputs.opam-overlays = {
    url = "github:dune-universe/opam-overlays";
    flake = false;
  };
  inputs.mirage-opam-overlays = {
    url = "github:dune-universe/mirage-opam-overlays";
    flake = false;
  };

  outputs = { self, nixpkgs, opam-nix, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        inherit (opam-nix.lib.${system}) buildOpamProject;

        build_ocaml_package = name: path:
          let
            scope = buildOpamProject {
              resolveArgs = {
                depopts = false;
                env.sys-ocaml-version = "4.14.2";
              };
            } name path { ocaml-system = "*"; };

          in scope.${name}.overrideAttrs (_: {
            # Prevent unnecessary dependencies on the resulting derivation
            removeOcamlReferences = true;
            doNixSupport = false;
          });

      in rec {
        packages = { rss_to_mail = build_ocaml_package "rss_to_mail" ./.; };
        defaultPackage = packages.rss_to_mail;

      });
}
