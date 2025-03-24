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
    perlnavigator # language server for Perl
    emmet-ls # language server for HTML etc
  ];
}
