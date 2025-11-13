{ inputs, pkgs, ... }:

imports = [
  ../all.nix
];

home.packages = with pkgs; [
  deno # JS runtime for yt-dlp
];
