{ lib, pkgs, ... }:

{
  programs = {
    home-manager.enable = true;
    zoxide.enable = true;
  };

  fonts.fontconfig.enable = true;

  nix = {
    package = lib.mkDefault pkgs.nix;
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
    pkgs.source-code-pro
  ];
}
