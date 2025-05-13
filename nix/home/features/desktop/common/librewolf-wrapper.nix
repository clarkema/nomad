{ pkgs, ... }:

# This is a simple wrapper to allow me to run librewolf from flatpak but still
# call it from the commandline in the same way as if it were installed via nix
# or the system package manager.  Especially useful for kagi.nix.
let
  librewolf-wrapper = pkgs.writeShellScriptBin "librewolf" ''
    exec flatpak run io.gitlab.librewolf-community "$@"
  '';
in
{
  home.packages = [
    librewolf-wrapper
  ];
}
