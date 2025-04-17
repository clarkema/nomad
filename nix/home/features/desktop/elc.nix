{ pkgs, nomad, ... }:

{
  imports = [
    ../dev/elixir.nix
  ];

  programs = {
    ripgrep.enable = true;
  };

  home.packages = with pkgs; [
    asdf-vm
    gh
    unzip
    yarn
    ijq
    perlnavigator # language server for Perl
    emmet-ls # language server for HTML etc
    jq
    skim
    neovim
    emacs-gtk
    emacsPackages.treesit-grammars.with-all-grammars
    tree-sitter
    nomad.tm
    eza
  ];
}
