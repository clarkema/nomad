{ inputs, pkgs, ... }: {
  imports = [ ../all.nix ];

  home.packages =
  let
    rakudo_env = pkgs.buildEnv {
      name = "rakudo-env";
      paths = [
        pkgs.readline
        pkgs.rakudo
        pkgs.zef
        pkgs.util-linux
        pkgs.zlib
      ];
      pathsToLink = [ "/bin" "/lib" "/share" ];
    };
  in with pkgs; [
    signal-desktop
    rakudo_env
    neovim
    emacsPackages.treesit-grammars.with-all-grammars
    vscodium
    chirp
  ];

  home.file.".local/share/applications/signal-desktop.desktop".source = "${pkgs.signal-desktop}/share/applications/signal-desktop.desktop";
}
