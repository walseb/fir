{ compiler ? "ghc883" }:
let
  nixOverride = ./nixpkgs.json;
  pkgs' = let spec = builtins.fromJSON (builtins.readFile nixOverride);
  in (builtins.fetchTarball {
    url = "${spec.url}/archive/${spec.rev}.tar.gz";
    inherit (spec) sha256;
  });
  overlayShared = import ./overlay.nix { inherit compiler; };
  config = {
    allowBroken = true; # easytensor is marked as broken.
    allowUnfree = true;
  };
  nixpkgs = import pkgs' {
    inherit config;
    overlays = [ overlayShared ];
  };
  base-compiler = nixpkgs.haskell.packages."${compiler}";
in base-compiler.fir
