{ pkgs ? import <nixpkgs> {}}:

pkgs.mkShell {
  packages = with pkgs; [ python3 gnumake gcc gdb clang-tools ];
  shellHook = ''
    export TMPDIR="/tmp"
  '';
}

