{ pkgs, ... }:

let binDir = "/Users/clarkema/.nix-profile/bin"; in
{
  home.packages = with pkgs; [
    tmux
    neovim
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs30-pgtk;
  };
}
