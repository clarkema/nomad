{ pkgs, ... }:

{
  programs = {
    ripgrep.enable = true;
  };

  home.packages = with pkgs; [
    tmux
    neovim
    emacs
  ];
}
