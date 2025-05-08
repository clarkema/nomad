{ pkgs, ... }:

{
  imports = [
    ./librewolf.nix
  ];

  home.file.".local/share/applications/kagi-asssitant.desktop".text =
    ''
    [Desktop Entry]
    Name=Kagi Assistant
    Comment=Open LibreWolf with Kagi Assistant
    Exec=librewolf "https://kagi.com/assistant"
    Icon=/home/clarkema/.local/share/icons/kagi/kagi.svg
    Type=Application
    Categories=Network;WebBrowser;
    X-KDE-Shortcuts=Ctrl+Meta+K
    '';
}
