{ pkgs, ... }:

let binDir = "/home/clarkema/.nix-profile/bin"; in
{
  home.packages = [
    pkgs.hyprland
    pkgs.wofi
    pkgs.light
  ];

  programs.waybar = {
    enable = true;
    package = pkgs.waybar.overrideAttrs (oa: {
      # Required to be able to use the wlr/workspaces module
      mesonFlags = (oa.mesonFlags or []) ++ [ "-Dexperimental=true" ];
    });
  };
}
