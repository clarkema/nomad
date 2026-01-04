{ inputs, pkgs, nomad, ... }:

{
  home.packages = with pkgs; [
    librewolf
    mpv
    ripgrep

    gcc
    gnumake

    _1password-gui
    xfce.thunar
    vlc

    nerd-fonts.ubuntu-mono
    font-awesome_5
    haskellPackages.xmonad-dbus
    polybarFull
    rofi
    picom

    nixfmt-rfc-style

    nomad.tm
  ];

  programs.neovim = {
    enable = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages =
      epkgs: with epkgs; [
        vterm
      ];
  };
}
