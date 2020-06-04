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
      rev = "859155b3608d45c2aae9500ce47d2a405ea0ab88";
      sha256 = "0x83b8dyc6npnqgv5i1aazq1chn98fzh4g3vdjg4sdm0zygvsj2x";
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
