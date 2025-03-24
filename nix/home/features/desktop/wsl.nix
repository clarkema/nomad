{ pkgs, nomad, ... }:

{
  programs = {
    ripgrep.enable = true;
  };

  home.packages = with pkgs; [
    tmux
    skim
    neovim
    emacs
    tree-sitter
    nomad.tm
    eza
  ];

  home.file.".config/ghostty/config".text =
    ''
    cursor-color = #ff0000
    cursor-style = block
    cursor-style-blink = false
    shell-integration-features = no-cursor
    theme = GruvboxDark
    font-family = Source Code Pro
    font-size = 16
    font-feature = -calt
    keybind = ctrl+shift+f10=toggle_window_decorations
    keybind = ctrl+shift+f11=toggle_fullscreen
    window-height = 50
    window-width = 150
    '';
}
