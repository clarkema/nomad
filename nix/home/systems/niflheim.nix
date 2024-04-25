{ inputs, pkgs, nomad, ... }:

let
  binDir = "/home/clarkema/.nix-profile/bin";
  myPkgs = [ nomad.tm ];
in
{
  imports = [ ../all.nix ];

  home.packages = with pkgs; [
    # mull
    # kitty
    # aspell
    # aspellDicts.en
    # aspellDicts.en-science
    # aspellDicts.en-computers

    #    ripgrep 
    nodejs_21 # for nvim coc

    emacs-gtk
    elixir_1_14
    emacsPackages.treesit-grammars.with-all-grammars
    mullvad-browser
    ungoogled-chromium
    _1password-gui
    signal-desktop
    welle-io
    nixgl.nixGLIntel
    python312
    distrobox
  ]
  ++ myPkgs;

  home.file.".local/share/applications/mullvad-browser.desktop".source = "${pkgs.mullvad-browser}/share/applications/mullvad-browser.desktop";

  home.file.".local/share/applications/chromium.desktop".source = "${pkgs.ungoogled-chromium}/share/applications/chromium-browser.desktop";

  home.file.".local/share/applications/1password.desktop".source = "${pkgs._1password-gui}/share/applications/1password.desktop";

  home.file.".local/share/applications/signal-desktop.desktop".source = "${pkgs.signal-desktop}/share/applications/signal-desktop.desktop";
}
