{ inputs, pkgs, ... }: {
  imports = [ ../all.nix ];

  home.packages = with pkgs; [
    kitty
    aspell
    aspellDicts.en
    aspellDicts.en-science
    aspellDicts.en-computers
    wdisplays # Wayland display manager
    ripgrep
    mpg123
  ];

  services.ssh-agent.enable = true;

  wayland.windowManager.sway = {
    enable = true;
    config = rec {
      modifier = "Mod4";
      terminal = "kitty";

      input = {
        "*" = {
          "xkb_layout" = "gb";
        };
      };

      colors = {
        background = "#ffffff";
      };

      output = {
        "*" = {
          bg = "#ffffff solid_color";
        };
      };

      bars = [
        {
          colors = {
            background = "#ffffff";
          };
        }
      ];
    };
    extraConfig = ''
      bindsym Mod4+Shift+p exec "miractl refresh"
    '';
  };
}
