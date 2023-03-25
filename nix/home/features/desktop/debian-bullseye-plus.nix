{ pkgs, ... }:

{
  home.packages = [
    pkgs.kitty
    pkgs.nixgl.nixGLIntel
  ];

  home.file.".local/share/applications/kitty.desktop".text =
    let binDir = "/home/clarkema/.nix-profile/bin"; in
    ''
    [Desktop Entry]
    Version=1.0
    Type=Application
    Name=kitty
    GenericName=Terminal emulator
    Comment=Fast, feature-rich, GPU based terminal
    Exec=${binDir}/nixGLIntel ${binDir}/kitty
    Categories=System;TerminalEmulator;
    '';
}
