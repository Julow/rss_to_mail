{ pkgs ? import <nixpkgs> {} }:

let
  opam_repo_rev = "290023ebc31af2ba926bf06cce9731f65bea42bf";

  args = {
    ocaml = pkgs.ocaml-ng.ocamlPackages_4_10.ocaml;
    selection = ./opam-selection.nix;
    src = ./.;
  };
in

rec {
  selection = pkgs.opam2nix.build args;
  resolve = pkgs.opam2nix.resolve args [
    "--repo-commit=${opam_repo_rev}" "./rss_to_mail.opam"
  ];
  inherit (selection) rss_to_mail;
}
