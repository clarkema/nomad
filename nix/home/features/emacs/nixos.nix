{ pkgs, ... }:

{
  home.packages = with pkgs; [
    aspell
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    extraPackages =
      epkgs: with epkgs; [
        vterm
      ];
  };
}
