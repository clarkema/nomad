{ inputs, pkgs, ... }:

let binDir = "/home/clarkema/.nix-profile/bin"; in
{
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

  home.file.".local/share/applications/chirp.desktop".text =
    ''
    [Desktop Entry]
    Type=Application
    Version=1.0
    Name=CHIRP
    GenericName=Radio Programming Tool
    Comment=Program amateur radios
    Icon=${pkgs.chirp}/lib/python3.11/site-packages/chirp/share/chirp
    Exec=${binDir}/chirp %F
    Terminal=false
    MimeType=x-scheme-handler/chirp
    Categories=Utility;HamRadio
    Keywords=Hamradio;Programming;Handheld;Radio;Amateur;Programmer
    StartupNotify=true
    }
    '';
}
