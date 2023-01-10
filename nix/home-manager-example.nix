{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    extraConfig = ''
      (require 'extended-faces)
    '';
    extraPackages = epkgs: [epkgs.extended-faces];
  };
}
