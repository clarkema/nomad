{ lib, pkgs, ... }:

{
  programs = {
    home-manager.enable = true;
    zoxide.enable = true;
  };

  fonts.fontconfig.enable = true;

  home.file.".config/nix/nix.conf".text = ''
    # Enable flakes and new command-line interface
    experimental-features = nix-command flakes
  '';

  home.packages = with pkgs; [
    pkgs.bat
    pkgs.btop
    pkgs.fd
    pkgs.fzf
    pkgs.htop
    pkgs.neofetch
    pkgs.fastfetch
    pkgs.scmpuff
    skim
    pkgs.source-code-pro

    pkgs.shellcheck
    pkgs.bash-language-server

    # Used by nvim config
    pkgs.nerd-fonts.sauce-code-pro

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

  home.file.".bashenv-nix".text =
    ''
    # This file is managed by Nix home-manager; do not edit directly.

    # Fix locale errors with Nix packages.
    # See https://nixos.wiki/wiki/Locales
    export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
    '';
}
