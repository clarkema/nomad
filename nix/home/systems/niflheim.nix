{ inputs, pkgs, ... }:

let binDir = "/home/clarkema/.nix-profile/bin"; in
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

    emacsPackages.treesit-grammars.with-all-grammars
    mullvad-browser
    ungoogled-chromium
  ];

  home.file.".local/share/applications/mullvad-browser.desktop".text =
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
}
