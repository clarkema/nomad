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
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system} = import ./shell.nix { inherit pkgs; };

      homeConfigurations = {
        "clarkema" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
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
          ];
        };
      };
    };
}
