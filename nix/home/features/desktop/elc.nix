{ pkgs, nomad, ... }:

{
  imports = [
    #../dev/elixir.nix
  ];

  home.packages = with pkgs; [
    asdf-vm
    gh
    unzip
    yarn
  ];
}
