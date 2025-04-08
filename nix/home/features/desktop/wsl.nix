{ pkgs, nomad, ... }:

{
  home.packages = with pkgs; [
    tmux
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

    for arg in "$@"; do
        # Check if the argument is a URL (starts with http:// or https://)
        if [[ "$arg" =~ ^https?:// ]]; then
            # It's a URL, pass it directly
            windows_args+=("$arg")
        else
            # It's a file path, convert it to a Windows path using wslpath
            windows_args+=("$(wslpath -w "$arg")")
        fi
    done

    exec '/mnt/c/Program Files/Google/Chrome/Application/chrome.exe' "''${windows_args[@]}"
    '';
    executable = true;
  };
}
