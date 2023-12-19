{ inputs, pkgs, ... }:

let binDir = "/home/clarkema/.nix-profile/bin"; in
{
  imports = [ ../all.nix ];

  home.packages = with pkgs; [
    # mull
    # kitty
    # aspell
    # aspellDicts.en
    # aspellDicts.en-science
    # aspellDicts.en-computers

    #    ripgrep 
    nodejs_21 # for nvim coc

    emacsPackages.treesit-grammars.with-all-grammars
    mullvad-browser
    ungoogled-chromium
  ];

  home.file.".local/share/applications/mullvad-browser.desktop".source = "${pkgs.mullvad-browser}/share/applications/mullvadbrowser.desktop";
}
