{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callCabal2nix "lyah-site" ./. {}
