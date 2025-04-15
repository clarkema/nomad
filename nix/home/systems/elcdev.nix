{ inputs, pkgs, ... }: {
  imports = [
    ../all.nix
    ../features/dev/elixir.nix
  ];

  home.packages = with pkgs; [
    signal-desktop
    ghostty
  ];

  # Add the home-manager bin directory to KDE's PATH so things like .desktop
  # files can find executables
#j  home.file.".config/plasma-workspace/env/path.sh" = {
 #j   executable = true;
  #j  text = ''
   #j export PATH=$HOME/.nix-profile/bin:$PATH
    #j'';
  #};


  home.file.".local/share/applications/ghostty.desktop".text =
    ''
    [Desktop Entry]
    Name=Ghostty
    Type=Application
    Comment=A terminal emulator
    Exec=${pkgs.nixgl.nixGLIntel}/bin/nixGLIntel ${pkgs.ghostty}/bin/ghostty
    Icon=com.mitchellh.ghostty
    Categories=System;TerminalEmulator;
    Keywords=terminal;tty;pty;
    StartupNotify=true
    Terminal=false
    Actions=new-window;
    X-GNOME-UsesNotifications=true
    '';

  home.file.".local/share/applications/signal-desktop.desktop".source = "${pkgs.signal-desktop}/share/applications/signal-desktop.desktop";

}

