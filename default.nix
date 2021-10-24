{ pkgs ? import <nixpkgs> { } }:

let
  opam_repo_rev = "882b3d139f5675109f782f1de8f79b358c5d734b";

  args = {
    ocaml = pkgs.ocaml-ng.ocamlPackages_4_10.ocaml;
    selection = ./opam-selection.nix;
    src = ./.;
  };

in rec {
  selection = pkgs.opam2nix.build args;
  resolve = pkgs.opam2nix.resolve args [
    "--repo-commit=${opam_repo_rev}"
    "./rss_to_mail.opam"
  ];
  inherit (selection) rss_to_mail;
}
