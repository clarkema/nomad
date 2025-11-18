{ pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    extraPackages =
      epkgs: with epkgs; [
        vterm
      ];
  };

  home.file.".local/share/applications/emacs.desktop".text =
    ''
    [Desktop Entry]
    Name=Emacs
    GenericName=Text Editor
    Comment=Edit text
    MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
    Exec=emacs %F
    Icon=emacs
    Type=Application
    Terminal=false
    Categories=Development;TextEditor;
    StartupNotify=true
    StartupWMClass=Emacs
    '';

  # GTK and pointerCursor theming is required to make the pointer in Emacs
  # match the system pointer and be visible. By default it has a black pointer
  # with a black border, which doesn't show up well.
  gtk = {
    enable = true;
  };

  home.pointerCursor = {
    #x11.enable = true;
    gtk.enable = true;
    package = pkgs.kdePackages.breeze;
    size = 24;
    name = "breeze_cursors";
  };
}
