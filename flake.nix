{
  description = "Additional generic faces for Emacs.";

  outputs = inputs:
    {
      overlays = {
        default = final: prev: {
          emacsPackagesFor = emacs:
            (prev.emacsPackagesFor emacs).overrideScope'
            (inputs.self.overlays.emacs final prev);
        };

        emacs = final: prev: efinal: eprev: {
          extended-faces = inputs.self.packages.${final.system}.default;
        };
      };
    }
    // inputs.flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [(import ./nix/dependencies.nix)];
      };

      emacsPath = package:
        "${package}/share/emacs/site-lisp/elpa/${package.ename}-${package.version}";

      src = pkgs.lib.cleanSource ./.;

      ## We need to tell Eldev where to find its Emacs package.
      ELDEV_LOCAL = emacsPath pkgs.emacsPackages.eldev;
    in {
      packages.default =
        inputs.bash-strict-mode.lib.checkedDrv pkgs
        (pkgs.stdenv.mkDerivation (let
          ename = "extended-faces";
          pname = "emacs-${ename}";
          version = "0.1.0";
        in {
          inherit ELDEV_LOCAL ename pname src version;

          nativeBuildInputs = [
            pkgs.emacs
            # Emacs-lisp build tool, https://doublep.github.io/eldev/
            pkgs.emacsPackages.eldev
          ];

          buildPhase = ''
            runHook preBuild
            eldev compile --warnings-as-errors
            runHook postBuild
          '';

          doCheck = true;

          checkPhase = ''
            runHook preCheck
            eldev test
            runHook postCheck
          '';

          installPhase = ''
            runHook preInstall
            eldev package
            mkdir -p "$out/share/emacs/site-lisp/elpa"
            tar --extract \
                --file="dist/${ename}-${version}.tar" \
                --directory "$out/share/emacs/site-lisp/elpa"
            runHook postInstall
          '';

          doInstallCheck = true;

          instalCheckPhase = ''
            runHook preInstallCheck
            eldev --packaged test
            runHook postInstallCheck
          '';
        }));

      devShells.default =
        ## TODO: Use `inputs.bash-strict-mode.lib.checkedDrv` here after
        ##       https://github.com/NixOS/nixpkgs/commit/58eb3d380601897c6ba9679eafc9c77305549b6f
        ##       makes it into a release.
        inputs.bash-strict-mode.lib.drv pkgs
        (pkgs.mkShell {
          inputsFrom =
            builtins.attrValues inputs.self.checks.${system}
            ++ builtins.attrValues inputs.self.packages.${system};

          nativeBuildInputs = [
            # Bash language server,
            # https://github.com/bash-lsp/bash-language-server#readme
            pkgs.nodePackages.bash-language-server
            # Nix language server,
            # https://github.com/nix-community/rnix-lsp#readme
            pkgs.rnix-lsp
          ];
        });

      checks = {
        doctor =
          inputs.bash-strict-mode.lib.checkedDrv pkgs
          (pkgs.stdenv.mkDerivation {
            inherit ELDEV_LOCAL src;

            name = "eldev-doctor";

            nativeBuildInputs = [
              pkgs.emacs
              # Emacs-lisp build tool, https://doublep.github.io/eldev/
              pkgs.emacsPackages.eldev
            ];

            buildPhase = ''
              runHook preBuild
              eldev doctor
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p "$out"
              runHook postInstall
            '';
          });

        lint =
          ## TODO: Can’t currently use `inputs.bash-strict-mode.lib.checkedDrv`
          ##       because the `emacs` wrapper script checks for existence of a
          ##       variable with `-n` intead of `-v`.
          inputs.bash-strict-mode.lib.shellchecked pkgs
          (pkgs.stdenv.mkDerivation {
            inherit ELDEV_LOCAL src;

            name = "eldev-lint";

            nativeBuildInputs = [
              pkgs.emacs
              pkgs.emacsPackages.eldev
            ];

            postPatch = ''
              {
                echo
                echo "(mapcar"
                echo " 'eldev-use-local-dependency"
                echo " '(\"${emacsPath pkgs.emacsPackages.dash}\""
                echo "   \"${emacsPath pkgs.emacsPackages.elisp-lint}\""
                echo "   \"${emacsPath pkgs.emacsPackages.package-lint}\""
                echo "   \"${emacsPath pkgs.emacsPackages.relint}\""
                echo "   \"${emacsPath pkgs.emacsPackages.xr}\"))"
              } >> Eldev
            '';

            buildPhase = ''
              runHook preBuild
              ## TODO: Currently needed to make a temp file in
              ##      `eldev--create-internal-pseudoarchive-descriptor`.
              export HOME="$PWD/fake-home"
              mkdir -p "$HOME"
              ## Need `--external` here so that we don’t try to download any
              ## package archives (which would break the sandbox).
              eldev --external lint doc elisp re
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p "$out"
              runHook preInstall
            '';
          });
      };

      # Nix code formatter, https://github.com/kamadorueda/alejandra#readme
      formatter = pkgs.alejandra;
    });

  inputs = {
    bash-strict-mode = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = github:sellout/bash-strict-mode;
    };

    flake-utils.url = github:numtide/flake-utils;

    nixpkgs.url = github:NixOS/nixpkgs/release-22.11;
  };
}
