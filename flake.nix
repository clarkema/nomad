{
 /*
  * See https://git.sr.ht/~misterio/nix-config
  */

  description = "My home configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nomad.url = "github:clarkema/nix-nomad";

    # Required for kitty in bullseye-plus
    nixgl.url = "github:nix-community/nixGL";

    # Old version of VSCodium for ESP-IDF
    nixesp.url = "github:clarkema/nixpkgs/929116e316068c7318c54eb4d827f7d9756d5e9c";
    mynixpkgs.url = "github:clarkema/nixpkgs";

    hyprland = {
      #type = "git";
      url = "git+https://github.com/hyprwm/hyprland?submodules=1";
      #submodules = true;
      inputs.nixpkgs.follows = "nixesp";
    };
  };

  outputs = { self, nixpkgs, home-manager, nixgl, nomad, ... }@inputs:
    let
      forAllSystems = function:
        nixpkgs.lib.genAttrs [
          "x86_64-linux"
          "aarch64-linux"
          "aarch64-darwin"
        ] (system: function nixpkgs.legacyPackages.${system});
    in {
      devShell = forAllSystems (pkgs:
        import ./shell.nix { inherit pkgs; }
      );
      #devShells.${system} = import ./shell.nix { inherit pkgs; };

      homeConfigurations = {
        "clarkema@niflheim.lfn.io" = home-manager.lib.homeManagerConfiguration {
          #inherit pkgs;
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
            overlays = [ nixgl.overlay ];
          };
          modules = [
            ./nix/home/features/dev/python.nix
            ./nix/home/systems/niflheim.nix
            {
              home = rec {
                username = "clarkema";
                homeDirectory = "/home/${username}";
                stateVersion = "22.11";
              };
              _module.args = { nomad = nomad.packages.x86_64-linux; };
            }
          ];
        };
        "clarkema@muninn" = home-manager.lib.homeManagerConfiguration {
          #inherit pkgs;
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
          };
          modules = [
            ./nix/home/systems/muninn.nix
            {
              home = rec {
                username = "clarkema";
                homeDirectory = "/home/${username}";
                stateVersion = "22.11";
              };
            }
          ];
        };
        "clarkema@vidar" = home-manager.lib.homeManagerConfiguration {
          #inherit pkgs;
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
            overlays = [ nixgl.overlay ];
          };
          extraSpecialArgs = { inherit inputs; };
          modules = [
            ./nix/home/features/dev/python.nix
            ./nix/home/features/desktop/debian-bookworm-plus.nix
            ./nix/home/systems/vidar.nix
            {
              home = rec {
                username = "clarkema";
                homeDirectory = "/home/${username}";
                stateVersion = "22.11";
              };
            }
          ];
        };
        "clarkema@njord" = home-manager.lib.homeManagerConfiguration {
          #inherit pkgs;
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
            overlays = [ nixgl.overlay ];
          };
          extraSpecialArgs = { inherit inputs; };
          modules = [
            ./nix/home/all.nix
            ./nix/home/systems/njord.nix
            {
              home = rec {
                username = "clarkema";
                homeDirectory = "/home/${username}";
                stateVersion = "22.11";
              };
              _module.args = { nomad = nomad.packages.x86_64-linux; };
            }
          ];
        };
        "clarkema@rat" = home-manager.lib.homeManagerConfiguration {
          #inherit pkgs;
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
          };
          extraSpecialArgs = { inherit inputs; };
          modules = [
            ./nix/home/systems/rat.nix
            {
              home = rec {
                username = "clarkema";
                homeDirectory = "/home/${username}";
                stateVersion = "22.11";
              };
            }
          ];
        };
        "clarkema@skadi.local" = home-manager.lib.homeManagerConfiguration {
          #inherit pkgs;
          pkgs = import nixpkgs {
            system = "aarch64-darwin";
            config.allowUnfree = true;
          };
          modules = [
            ./nix/home/systems/skadi.nix

            {
              home = rec {
                username = "clarkema";
                homeDirectory = "/Users/${username}";
                stateVersion = "22.11";
              };
            }
          ];
        };
        "clarkema@ELC-1XZ73M3" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs { system = "x86_64-linux"; };
          modules = [
            ./nix/home/all.nix
            ./nix/home/features/desktop/wsl.nix
            ./nix/home/features/desktop/elc.nix
            {
              home = rec {
                username = "clarkema";
                homeDirectory = "/home/${username}";
                stateVersion = "22.11";
              };
              _module.args = { nomad = nomad.packages.x86_64-linux; };
            }
          ];
        };
        "clarkema@elcdev" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs { system = "x86_64-linux"; };
          modules = [
            ./nix/home/all.nix
#            ./nix/home/features/desktop/ubuntu-24_04-plus.nix
            ./nix/home/features/desktop/elc.nix
            {
              home = rec {
                username = "clarkema";
                homeDirectory = "/home/${username}";
                stateVersion = "22.11";
              };
              _module.args = { nomad = nomad.packages.x86_64-linux; };
            }
          ];
        };
        "clarkema" = home-manager.lib.homeManagerConfiguration {
          #inherit pkgs;
          pkgs = import nixpkgs { system = "x86_64-linux"; };
          modules = [
            ./nix/home/all.nix
            {
              home = rec {
                username = "clarkema";
                homeDirectory = "/home/${username}";
                stateVersion = "22.11";
              };
            }
          ];
        };
      };
    };
}
