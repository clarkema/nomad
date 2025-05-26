.PHONY: test
test:
	nix run github:nix-community/nixos-generators -- --run -f vm --disk-size 20480 \
		-I nixpkgs=channel:nixos-25.05 -c yubikey-image.nix  

iso:
	nix run github:nix-community/nixos-generators -- -f iso -I nixpkgs=channel:nixos-25.05 \
		-c yubikey-image.nix
