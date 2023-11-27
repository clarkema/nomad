{ inputs, pkgs, ... }: {
  imports = [ ../all.nix ];

  home.packages = with pkgs; [
    signal-desktop
  ];
}
