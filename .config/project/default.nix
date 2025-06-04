{
  config,
  flaky,
  lib,
  supportedSystems,
  ...
}: {
  project = {
    name = "extended-faces";
    summary = "Inheritance-based faces for Emacs";
  };

  ## dependency management
  services.renovate.enable = true;

  ## development
  programs = {
    direnv.enable = true;
    git.enable = true;
  };

  ## formatting
  editorconfig.enable = true;
  programs = {
    treefmt.enable = true;
    vale = {
      enable = true;
      vocab.${config.project.name}.accept = [
        "dired"
        "docstring"
        "Fira"
        "speedbar"
      ];
    };
  };

  ## CI
  services.garnix.enable = true;

  ## publishing
  services.flakehub = {
    enable = true;
    name = lib.mkForce "sellout/emacs-extended-faces";
  };
  services.github.enable = true;
}
