{

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixos-generators.url = "github:nix-community/nixos-generators";
  };

  outputs = { self, nixpkgs, nixos-generators }:
  let
    system = "x86_64-linux";
  in
  {
    packages.${system}.zfs-rescue-iso =
      nixos-generators.nixosGenerate {
        inherit system;
        format = "iso";

        modules = [
          # Base installer ISO
          "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"

          ({ config, pkgs, ... }: {

            # ✅ Enable ZFS properly
            boot.supportedFilesystems = [ "zfs" ];

            # ✅ Ensure kernel matches ZFS
            boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;

            # Optional but recommended for rescue
            networking.hostId = "deadbeef"; # required for ZFS import

            # Extra rescue tools
            environment.systemPackages = with pkgs; [
              zfs
              zfs-prune-snapshots
              parted
              gptfdisk
              cryptsetup
              lsof
              strace
              vim
              git
            ];

            # Avoid running zed daemon in rescue
            services.zfs.zed.enable = false;

            # Allow importing pools without force-importing root
            boot.zfs.forceImportRoot = false;
          })
        ];
      };
  };
}

