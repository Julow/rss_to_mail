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

  outputs = { self, nixpkgs, opam-nix, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        inherit (opam-nix.lib.${system}) buildOpamProject;
        scope = buildOpamProject { } "rss_to_mail" ./. { ocaml-system = "*"; };

        # Prevent unnecessary dependencies on the resulting derivation
        rss_to_mail = scope.rss_to_mail.overrideAttrs (_: {
          removeOcamlReferences = true;
          doNixSupport = false;
        });
      in {
        packages = { inherit rss_to_mail; };
        defaultPackage = rss_to_mail;

      });
}
