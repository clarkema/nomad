{
  pkgs,
  ...
}:

{
  home.packages = with pkgs; [
    ripgrep
    skim

    (writeShellScriptBin "rgc" (builtins.readFile ../../../../bin/rgc))
  ];
}
