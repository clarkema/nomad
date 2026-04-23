{
  pkgs,
  ...
}:

{
  home.file.".zshrc".source = ../../../../dotfiles/.zshrc;
  home.file.".zshenv".source = ../../../../dotfiles/.zshenv;
  home.file.".nomad/sh/aliases".source = ../../../../sh/aliases;
}
