{ inputs, pkgs, ... }:

let
  hyprpkgs = import inputs.hyprland {
    system = "x86_64-linux";
  };
in
{
  home.packages = [
    pkgs.wofi
    pkgs.light
  ];

  programs.waybar = {
    enable = true;
    package = pkgs.waybar.overrideAttrs (oa: {
      # Required to be able to use the wlr/workspaces module
      mesonFlags = (oa.mesonFlags or []) ++ [ "-Dexperimental=true" ];
    });
  };

  wayland.windowManager.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    settings = {
      "$mod" = "SUPER";
      bind = [
        "$mod, F, exec, /home/clarkema/.nix-profile/bin/kitty"
        "$mod, B, exec, firefox"

      ];
    };
    extraConfig = ''
      $menu = wofi --show drun
      input {
        kb_layout = gb
      }

      animations {
        enabled = false
      }

      decoration {
        blur {
          enabled = false
        }
        shadow {
          enabled = false
        }
      }

      misc {
        vfr = true
        disable_hyprland_logo = true
      }

      bind = $mod, W, killactive
      bind = $mod, V, togglefloating
      bind = $mod, space, exec, $menu

      # Move / resize windows with mod+LMB/RMB
      bindm = $mod, mouse:272, movewindow
      bindm = $mod, mouse:273, resizewindow
    '';
  };
}
