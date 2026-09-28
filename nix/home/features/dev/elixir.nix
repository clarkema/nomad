{ pkgs, ... }: {
  home.packages = with pkgs; [
    beamPackages.elixir
    elixir-ls
  ];
}
