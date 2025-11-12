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
    fx # json viewer, can search by field name
    gh
    unzip
    yarn
    ijq
    perlnavigator # language server for Perl
    emmet-ls # language server for HTML etc
    jq
    neovim
    emacs-gtk
    emacs.pkgs.treesit-grammars.with-all-grammars
    emacs.pkgs.vterm
    tree-sitter
    nomad.tm
    eza
  ];
}
