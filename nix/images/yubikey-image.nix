# NixOS livesystem to generate yubikeys in an air-gapped manner
# screenshot: https://dl.thalheim.io/ZF5Y0yyVRZ_2MWqX2J42Gg/2020-08-12_16-00.png
# $ nixos-generate -f iso -c yubikey-image.nix
# $ nix run github:nix-community/nixos-generators -- -f iso -c yubikey-image.nix
#
# to test it in a vm:
#
# $ nixos-generate --run -f vm -c yubikey-image.nix
# $ nix run github:nix-community/nixos-generators -- --run -f vm  --disk-size 20480 -c yubikey-image.nix
#
# Originally from 
# https://github.com/Mic92/dotfiles/blob/a41e9c1722f7e81af21741ea75ced9ceff46230e/nixos/images/yubikey-image.nix

{ pkgs, ... }:

let
  guide = pkgs.stdenv.mkDerivation {
    name = "yubikey-guide-2020-08-12.html";
    src = pkgs.fetchFromGitHub {
      owner = "drduh";
      repo = "YubiKey-Guide";
      rev = "ece9752967e8b01bb3e70919a8ccdbc252eb9387";
      sha256 = "sha256-iqeUkEK2yrMi6dTpStEYZ3H7PoI5RoK3i90BRKZNlq8=";
    };
    buildInputs = [ pkgs.pandoc ];
    installPhase = "pandoc --highlight-style pygments -s --toc README.md -o $out";
  };
in {
  environment.interactiveShellInit = ''
    export GNUPGHOME=/run/user/$(id -u)/gnupghome
    if [ ! -d $GNUPGHOME ]; then
      mkdir $GNUPGHOME
    fi
    cp ${pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/drduh/config/75ec3f35c6977722d4dba17732d526f704f256ff/gpg.conf";
      sha256 = "sha256-LK29P4+ZAvy9ObNGDNBGP/8+MIUY3/Uo4eJtXhwMoE0=";
    }} "$GNUPGHOME/gpg.conf"
    echo "\$GNUPGHOME has been set up for you. Generated keys will be in $GNUPGHOME."
  '';

  environment.systemPackages = with pkgs; [
    yubikey-personalization
    yubikey-manager
#    yubikey-manager-qt
    yubioath-flutter
    cryptsetup
    pwgen
    #midori
    librewolf
    paperkey
    gnupg
    ctmg
  ];

  system.stateVersion = "25.05";

  services.udev.packages = with pkgs; [ yubikey-personalization ];
  services.pcscd.enable = true;

  # make sure we are air-gapped
  networking.wireless.enable = false;
  networking.dhcpcd.enable = false;

  services.getty.helpLine = "The 'root' account has an empty password.";

  security.sudo.wheelNeedsPassword = false;
  users.users.yubikey = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    shell = "/run/current-system/sw/bin/bash";
  };

  services.displayManager = {
    autoLogin.enable = true;
    autoLogin.user = "yubikey";
    defaultSession = "xfce";
  };

  services.xserver = {
    enable = true;
    displayManager.sessionCommands = ''
      ${pkgs.librewolf}/bin/librewolf ${guide} &
      ${pkgs.xfce.xfce4-terminal}/bin/xfce4-terminal &
    '';

    desktopManager = {
      xterm.enable = false;
      xfce.enable = true;
    };
  };
}
