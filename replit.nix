{ pkgs }: {
  deps = [
    pkgs.vim
    pkgs.gnatPackages.gnat
    pkgs.bashInteractive
    pkgs.nodePackages.bash-language-server
    pkgs.man
  ];
}