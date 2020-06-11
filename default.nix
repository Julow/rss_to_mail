{ pkgs ? import <nixpkgs> {} }:

let
  pinned_mrmime = pkgs.fetchgit {
    url = "https://github.com/mirage/mrmime";
    rev = "846c6a7f4b9702db2661ebb3fbb881f5f3240349";
    sha256 = "1gm30i5xzsw381kh4raqyph2r8f06m6nx5nbivi6rbviqj2rxzyw";
  };

  opam_repo_rev = "dbbd0be11808e3ff74e69c3cc0ee5889684cf7bb";

  args = {
    ocaml = pkgs.ocaml-ng.ocamlPackages_4_10.ocaml;
    selection = ./opam-selection.nix;
    src = ./.;
    override = {}: {
      mrmime = super: super.overrideAttrs (a: a // { src = pinned_mrmime; });
    };
  };
in

rec {
  selection = pkgs.opam2nix.build args;
  resolve = pkgs.opam2nix.resolve args [
    "--repo-commit=${opam_repo_rev}" "./rss_to_mail.opam" "${pinned_mrmime}/mrmime.opam"
  ];
  inherit (selection) rss_to_mail;
}
