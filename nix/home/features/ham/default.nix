{ pkgs, ... }:

{
  home.packages = with pkgs; [
    js8call
    pat
    wsjtx
  ];
}
