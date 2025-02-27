{ inputs, pkgs, ... }:

let
  binDir = "/home/clarkema/.nix-profile/bin";
  oldpkgs = import inputs.nixesp {
    system = "x86_64-linux";
    config.allowUnfree = true;
  };
  mypkgs = import inputs.mynixpkgs {
    system = "x86_64-linux";
    config.allowUnfree = true;
  };
in
{
  imports = [
    ../all.nix
    ../features/dev/elixir.nix
  ];

  programs.vscode = {
    enable = true;
    package = oldpkgs.vscodium;
    profiles.default.extensions = with pkgs.vscode-extensions; [
      oldpkgs.vscode-extensions.github.copilot-chat
      oldpkgs.vscode-extensions.github.copilot
      mypkgs.vscode-extensions.espressif.vscode-esp-idf-extension
      ms-vscode.cpptools
    ];
  };

  home.packages =
  let
    rakudo_env = pkgs.buildEnv {
      name = "rakudo-env";
      paths = [
        pkgs.readline
        pkgs.rakudo
        pkgs.zef
        pkgs.util-linux
        pkgs.zlib
      ];
      pathsToLink = [ "/bin" "/lib" "/share" ];
    };
  in with pkgs; [
    signal-desktop
    rakudo_env
    neovim
    emacsPackages.treesit-grammars.with-all-grammars
    chirp
    ghostty
    clojure
    clj-kondo
  ];

  # Add the home-manager bin directory to KDE's PATH so things like .desktop
  # files can find executables
  home.file.".config/plasma-workspace/env/path.sh" = {
    executable = true;
    text = ''
    export PATH=$HOME/.nix-profile/bin:$PATH
    '';
  };

  home.file.".local/share/applications/codium.desktop".text =
    ''
    [Desktop Entry]
    Actions=new-empty-window
    Categories=Utility;TextEditor;Development;IDE
    Comment=Code Editing. Redefined.
    Exec=${pkgs.vscodium}/bin/codium %F
    GenericName=Text Editor
    Icon=${pkgs.vscodium}/share/pixmaps/vscodium
    Keywords=vscode
    Name=VSCodium
    StartupNotify=true
    StartupWMClass=vscodium
    Type=Application
    Version=1.4

    [Desktop Action new-empty-window]
    Exec=${pkgs.vscodium}/bin/codium --new-window %F
    Icon=${pkgs.vscodium}/share/pixmaps/vscodium
    Name=New Empty Window
    '';

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

  home.file.".local/share/applications/chirp.desktop".text =
    ''
    [Desktop Entry]
    Type=Application
    Version=1.0
    Name=CHIRP
    GenericName=Radio Programming Tool
    Comment=Program amateur radios
    Icon=${pkgs.chirp}/lib/python3.11/site-packages/chirp/share/chirp
    Exec=${binDir}/chirp %F
    Terminal=false
    MimeType=x-scheme-handler/chirp
    Categories=Utility;HamRadio
    Keywords=Hamradio;Programming;Handheld;Radio;Amateur;Programmer
    StartupNotify=true
    }
    '';

  home.file.".config/mpv/mpv.conf".text =
    ''
    # Managed by home-manager
    hwdec=auto
    '';

  # https://blog.tty8.org/posts/2020-05-18-howto-auto-start-ssh-agent-with-systemd-on-debian-bullseye.html
  home.file.".config/systemd/user/ssh-agent.service".text =
    ''
    # Managed by home-manager
    [Unit]
    Description=SSH key agent

    [Service]
    Type=simple
    # %t resolves to XDG_RUNTIME_DIR; see SPECIFIERS section in systemd.unit(5)
    ExecStart=/usr/bin/ssh-agent -D -a "%t/ssh-agent.socket"

    [Install]
    WantedBy=default.target
    '';


  home.file.".pam_environment".text =
    ''
    SSH_AUTH_SOCK DEFAULT="''${XDG_RUNTIME_DIR}/ssh-agent.socket"
    '';

}
