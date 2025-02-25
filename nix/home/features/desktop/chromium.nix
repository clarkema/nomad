{ pkgs, ... }:

let binDir = "/home/clarkema/.nix-profile/bin"; in
{
  home.packages = with pkgs; [
    nixgl.nixGLIntel
    ungoogled-chromium
  ];

  home.file.".local/share/applications/chromium.desktop".text =
    ''
    [Desktop Entry]
    StartupWMClass=chromium-browser
    Version=1.0
    Name=Chromium
    # Only KDE 4 seems to use GenericName, so we reuse the KDE strings.
    # From Ubuntu's language-pack-kde-XX-base packages, version 9.04-20090413.
    GenericName=Web Browser
    Comment=Access the Internet
    Exec=${pkgs.nixgl.nixGLIntel}/bin/nixGLIntel ${binDir}/chromium %U
    StartupNotify=true
    Terminal=false
    Icon=chromium
    Type=Application
    Categories=Network;WebBrowser;
    MimeType=application/pdf;application/rdf+xml;application/rss+xml;application/xhtml+xml;application/xhtml_xml;application/xml;image/gif;image/jpeg;image/png;image/webp;text/html;text/xml;x-scheme-handler/http;x-scheme-handler/https;x-scheme-handler/webcal;x-scheme-handler/mailto;x-scheme-handler/about;x-scheme-handler/unknown
    Actions=new-window;new-private-window;
    '';
}
