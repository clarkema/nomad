{ inputs, pkgs, ... }: {
  imports = [
    ../all.nix
    ../features/desktop/macos-plus.nix
  ];

  home.packages = with pkgs; [
  ];
}
