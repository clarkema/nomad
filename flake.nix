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
    nixgl.url = "github:guibou/nixGL";
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
        "clarkema@skadi" = home-manager.lib.homeManagerConfiguration {
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
