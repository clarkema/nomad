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

    # Required for kitty in bullseye-plus
    nixgl.url = "github:guibou/nixGL";
  };

  outputs = { self, nixpkgs, home-manager, nixgl, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ nixgl.overlay ];
      };
    in {
      devShells.${system} = import ./shell.nix { inherit pkgs; };

      homeConfigurations = {
        "clarkema@munnin" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [
            ./nix/home/system/munnin.nix
            {
              home = rec {
                username = "clarkema";
                homeDirectory = if pkgs.system == "aarch64-darwin"
                       then "/Users/${username}"
                       else "/home/${username}";

                stateVersion = "22.11";
              };
            }
	  ];
	};
        "clarkema@generic" = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [
            ./nix/home/all.nix
            {
              home = rec {
                username = "clarkema";
                homeDirectory = if pkgs.system == "aarch64-darwin"
                       then "/Users/${username}"
                       else "/home/${username}";

                stateVersion = "22.11";
              };
            }
            ./nix/home/features/desktop/debian-bullseye-plus.nix
            ./nix/home/features/desktop/debian-bookworm-plus.nix
            ./nix/home/features/desktop/hyprland-test.nix
          ];
        };
      };
    };
}
