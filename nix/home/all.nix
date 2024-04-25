{ lib, pkgs, ... }:

{
  programs = {
    home-manager.enable = true;
    zoxide.enable = true;
  };

  fonts.fontconfig.enable = true;

  nix = {
    package = pkgs.nixFlakes;
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      warn-dirty = false;
    };
  };

  home.packages = [
    pkgs.bat
    pkgs.btop
    pkgs.fd
    pkgs.fzf
    pkgs.scmpuff
    pkgs.yt-dlp
    pkgs.neofetch
    pkgs.source-code-pro

    # Used by org-present in my Emacs config
    (pkgs.iosevka-bin.override { variant = "Aile"; })
    (pkgs.iosevka-bin.override { variant = "Etoile"; })
  ];

  home.file.".zshenv-nix".text =
    ''
    # This file is managed by Nix home-manager; do not edit directly.

    # Fix locale errors with Nix packages.
    # See https://nixos.wiki/wiki/Locales
    export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
    '';
}
