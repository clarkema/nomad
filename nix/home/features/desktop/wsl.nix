{ pkgs, nomad, ... }:

{
  programs = {
    ripgrep.enable = true;
  };

  home.packages = with pkgs; [
    jq
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


  home.file.".nomad/bin/chrome" = {
    text = ''
    #! /usr/bin/env bash

    exec '/mnt/c/Program Files/Google/Chrome/Application/chrome.exe' "$@"
    '';
    executable = true;
  };
}
