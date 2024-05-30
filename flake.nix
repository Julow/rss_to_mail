{
  inputs.nixpkgs.url = "nixpkgs";
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
  inputs.hillingar = {
    url = "github:RyanGibb/hillingar";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.opam-repository.follows = "opam-repository";
    inputs.opam-overlays.follows = "opam-overlays";
    inputs.opam-nix.follows = "opam-nix";
    inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, opam-nix, flake-utils, hillingar, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        inherit (opam-nix.lib.${system}) buildOpamProject;
        inherit (hillingar.lib.${system}) mkUnikernelPackages;

        queryArgs = {
          resolveArgs = {
            depopts = false;
            env.sys-ocaml-version = "4.14.1";
          };
        };

        build_ocaml_package = name: path:
          let
            scope =
              buildOpamProject queryArgs name path { ocaml-system = "*"; };

          in scope.${name}.overrideAttrs (_: {
            # Prevent unnecessary dependencies on the resulting derivation
            removeOcamlReferences = true;
            doNixSupport = false;
          });

        build_unikernel = unikernelName: target: mirageDir:
          (mkUnikernelPackages rec {
            inherit unikernelName mirageDir;
            monorepoQuery = { ocaml-base-compiler = "4.14.1"; };
            query = {
              ocaml-base-compiler = "4.14.1";
              mirage = "4.5.0";
            };
          } ./.).${target};

      in rec {
        packages = {
          rss_to_mail = build_ocaml_package "rss_to_mail" ./.;
          rss_to_mail-hvt = build_unikernel "rss_to_mail" "hvt" "bin/mirage";
        };
        defaultPackage = packages.rss_to_mail;

      });
}
