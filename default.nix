{ pkgs ? import <nixpkgs> {} }:

let
  args = {
    ocaml = pkgs.ocaml-ng.ocamlPackages_4_10.ocaml;
    selection = ./opam-selection.nix;
    src = ./.;
  };
in

rec {
  selection = pkgs.opam2nix.build args;
  resolve = pkgs.opam2nix.resolve args [ "./rss_to_mail.opam" ];
  inherit (selection) rss_to_mail;
}
