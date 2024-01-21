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

  programs.firefox.enable = true;

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
            focusedWorkspace = {
              background = "#222222";
              border = "#222222";
              text = "#ffffff";
            };
            inactiveWorkspace = {
              background = "#ffffff";
              border = "#ffffff";
              text = "#000000";
            };
          };
        }
      ];
    };
    extraConfig = ''
      bindsym Mod4+Shift+p exec "miractl refresh"
      bindsym Mod4+Shift+b exec "firefox"
    '';
  };
}
