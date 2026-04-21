{
  pkgs,
  ...
}:

{
  home.packages = with pkgs; [
    ripgrep
    skim

    (writeShellScriptBin "rgc" (builtins.readFile ../../../../bin/rgc))
    (writeShellScriptBin "preview.sh" (builtins.readFile ../../../../bin/preview.sh))
  ];
}
