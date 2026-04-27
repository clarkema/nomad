{
  inputs,
  pkgs,
  nomad-pkgs,
  ...
}:

{
  _module.args.pkgsUnstable = pkgs;
  imports = [
    ../features/desktop/common/clipboard.nix
    ../features/ham
    ../features/emacs/nixos.nix
    ../features/neovim
    ../features/tmux
  ];

  programs.vscode = {
    enable = true;
    package = pkgs.vscode;

    profiles.default.userSettings = {
      "telemetry.telemetryLevel" = "off";
      "telemetry.enableTelemetry" = false;
      "telemetry.enableCrashReporter" = false;
      "extensions.autoUpdate" = false; # also worth disabling for reproducibility
      "update.mode" = "none"; # prevent VS Code from self-updating
    };

    # You can also pin extensions declaratively here
    profiles.default.extensions = with pkgs.vscode-extensions; [
      ms-vscode.vscode-speech
      ms-vscode-remote.remote-ssh
      anthropic.claude-code
    ];
  };

  home.packages = with pkgs; [
    librewolf
    mpv

    gcc
    gnumake

    _1password-gui
    thunar
    vlc

    nerd-fonts.ubuntu-mono
    font-awesome_5
    haskellPackages.xmonad-dbus
    polybarFull
    rofi
    picom

    nixfmt

    nomad-pkgs.pcal
    nomad-pkgs.git-map
    gammastep
    signal-desktop

    obsidian
    bruno
  ];

  programs.neovim = {
    enable = true;
  };

}
