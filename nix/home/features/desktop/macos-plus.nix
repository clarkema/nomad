{ pkgs, ... }:

let binDir = "/Users/clarkema/.nix-profile/bin"; in
{
  home.packages = with pkgs; [
    tmux
    neovim
  ];
}
