{ pkgs ? import <nixpkgs> {}}:

pkgs.mkShell {
  packages = with pkgs; [ gnumake gcc gdb clang-tools ];
}

