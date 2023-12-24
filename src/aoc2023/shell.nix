{ pkgs ? import <nixpkgs> { } }:

let
  pythonEnv = pkgs.python3.withPackages(ps: [ps.numpy]);

in
pkgs.mkShell {
  packages = [
    pythonEnv
  ];
}
