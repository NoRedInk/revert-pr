{ sources ? import ./nix/sources.nix }:
let
  overlays =
    [ (import sources.nix-script).overlay."${builtins.currentSystem}" ];
  pkgs = import sources.nixpkgs { overlays = overlays; };
in pkgs.stdenv.mkDerivation {
  name = "revert-pr";
  buildInputs = [ pkgs.nix-script-haskell ];
  src = ./.;
  installPhase = ''
    mkdir -p $out/bin
    mv ./revert-pr.hs $out/bin/revert-pr
    chmod a+x $out/bin/revert-pr
  '';
}
