{ pkgs, nomad-pkgs, ... }:

{
  home.packages = with pkgs; [
    tmux
    nomad-pkgs.tm
  ];

  xdg.configFile."tmux/tmux.conf" = {
    source = ../../../../dotfiles/.tmux.conf;
  };
}
