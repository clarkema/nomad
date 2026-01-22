{ pkgs, ... }:

{
  home.packages = with pkgs; [
    wezterm
  ];

  home.file.".local/share/applications/wezterm.desktop".text = ''
    [Desktop Entry]
    Name=Wezterm
    Type=Application
    Comment=A terminal emulator
    Exec=${pkgs.nixgl.nixGLIntel}/bin/nixGLIntel ${pkgs.wezterm}/bin/wezterm
    Icon=org.wezfurlong.wezterm
    Categories=System;TerminalEmulator;
    Keywords=terminal;tty;pty;
    StartupNotify=true
    Terminal=false
    Actions=new-window;
    X-GNOME-UsesNotifications=true
  '';
}
