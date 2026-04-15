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
    gammastep
    signal-desktop

    obsidian
    bruno
  ];

  programs.neovim = {
    enable = true;
  };

}
