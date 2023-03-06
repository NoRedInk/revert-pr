{ sources ? import ./nix/sources.nix }:
let
  pkgs = import sources.nixpkgs { };
  env = import ./default.nix { };
in pkgs.mkShell { buildInputs = env.buildInputs; }
