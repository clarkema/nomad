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

  home.file.".local/share/applications/chromium.desktop".source = "${pkgs.ungoogled-chromium}/share/applications/chromium-browser.desktop";

  home.file.".local/share/applications/1password.desktop".source = "${pkgs._1password-gui}/share/applications/1password.desktop";

  home.file.".local/share/applications/signal-desktop.desktop".source = "${pkgs.signal-desktop}/share/applications/signal-desktop.desktop";

  home.file.".local/share/applications/mullvad-browser.desktop" = {
    text =
      ''
      [Desktop Entry]
      StartupWMClass=mullvad-browser
      Version=1.0
      Name=Mullvad Browser
      # Only KDE 4 seems to use GenericName, so we reuse the KDE strings.
      # From Ubuntu's language-pack-kde-XX-base packages, version 9.04-20090413.
      GenericName=Web Browser
      Comment=Access the Internet
      Exec=${binDir}/mullvad-browser %U
      StartupNotify=true
      Terminal=false
      Icon=mullvad-browser
      Type=Application
      Categories=Network;WebBrowser;
      MimeType=application/pdf;application/rdf+xml;application/rss+xml;application/xhtml+xml;application/xhtml_xml;application/xml;image/gif;image/jpeg;image/png;image/webp;text/html;text/xml;x-scheme-handler/http;x-scheme-handler/https;x-scheme-handler/webcal;x-scheme-handler/mailto;x-scheme-handler/about;x-scheme-handler/unknown
      Actions=new-window;new-private-window;
      '';
    executable = true;
  };
}
