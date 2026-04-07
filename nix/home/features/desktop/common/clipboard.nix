{ pkgs, ... }:

{
  home.packages = with pkgs; [
    xclip
    (writeShellScriptBin "pbcopy" "exec xclip -selection clipboard \"$@\"")
    (writeShellScriptBin "pbpaste" "exec xclip -selection clipboard -o \"$@\"")
  ];
}
