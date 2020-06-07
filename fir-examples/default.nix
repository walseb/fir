{ compiler ? "ghc883" }:
let
  nixOverride = ../nixpkgs.json;
  pkgs' = let spec = builtins.fromJSON (builtins.readFile nixOverride);
  in (builtins.fetchTarball {
    url = "${spec.url}/archive/${spec.rev}.tar.gz";
    inherit (spec) sha256;
  });
  nixpkgs' = import pkgs' { };
  overlayShared = import ../overlay.nix { inherit compiler; };
  config = {
    allowBroken = true; # easytensor is marked as broken.
    allowUnfree = true;
  };
  sources = {
    vulkan-darwin = import (nixpkgs'.fetchgit {
      url =
        "https://github.com/o1lo01ol1o/vulkan.git"; # vulkan-api exposing moltenvk overlay for darwin
      rev = "f5c3bead73b5d06a6c9b8ccffdf1ef9bec80f195";
      sha256 = "0zmh2h6zs9bp2qg5nf29lw4dhw0lkd1f2hkgbqd9d0cnpy76vyag";
    }) { inherit pkgs' compiler; };

  };

  overlays = if nixpkgs'.stdenv.isDarwin then [
    overlayShared
    sources.vulkan-darwin.overlayShared
  ] else
    [ overlayShared ];

  pkgs = import pkgs' { inherit config overlays; };

  base-compiler = pkgs.haskell.packages."${compiler}";
  fir-examples' = base-compiler.callCabal2nix "fir-examples" ./. {
    inherit (base-compiler) fir;
  };

  fir-examples = if nixpkgs'.stdenv.isDarwin then
    pkgs.lib.overrideDerivation fir-examples' sources.vulkan-darwin.moltenHooks
  else
    fir-examples';

in { inherit overlayShared fir-examples; }
