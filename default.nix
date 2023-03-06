{ sources ? import ./nix/sources.nix }:
let
  overlays =
    [ (import sources.nix-script).overlay."${builtins.currentSystem}" ];
  pkgs = import sources.nixpkgs { overlays = overlays; };
in pkgs.stdenv.mkDerivation {
  name = "revert-pr";
  buildInputs = [
    pkgs.nix-script-haskell
    pkgs.fzf
    pkgs.haskellPackages.turtle
    pkgs.haskellPackages.aeson
  ];
}
