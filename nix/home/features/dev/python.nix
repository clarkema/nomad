{ pkgs, ... }:

{
  home.packages = with pkgs; [
    ruff
    pyright
  ];
}
