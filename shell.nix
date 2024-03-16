{ pkgs ? import <nixpkgs> { } }:

pkgs.buildNimPackage {
  name = "dummy";
  lockFile = ./lock.json;
}
