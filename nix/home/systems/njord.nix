{
  inputs,
  pkgs,
  nomad,
  ...
}:

{
  imports = [
    ../features/ham
  ];

  home.packages = with pkgs; [
    librewolf
    mpv
    ripgrep

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

    nomad.tm
    nomad.pcal
    gammastep
    signal-desktop
  ];

  programs.neovim = {
    enable = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    extraPackages =
      epkgs: with epkgs; [
        vterm
      ];
  };
}
