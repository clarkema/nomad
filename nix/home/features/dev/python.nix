{ pkgs, ... }: {
  home.packages = with pkgs; [
    ruff-lsp
    pyright
  ];
}
