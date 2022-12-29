{
  description = "Additional generic faces for Emacs.";

  outputs = inputs:
    {
      overlays = {
        default = final: prev: {
          emacsPackagesFor = emacs:
            (prev.emacsPackagesFor emacs).overrideScope'
            inputs.self.overlays.emacs;
        };

        emacs = final: prev: {
          extended-faces = inputs.self.packages.${final.system}.default;
        };
      };
    }
    // inputs.flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [(import ./nix/dependencies.nix)];
      };

      src = pkgs.lib.cleanSource ./.;

      ## Eldev wants to use `HOME`, so we point it to a fake one within the
      ## build.
      relocateHome = ''
        export HOME="$PWD/fake-home"
        mkdir -p "$HOME"
      '';
    in {
      packages.default =
        inputs.bash-strict-mode.lib.checkedDrv pkgs
        (pkgs.stdenv.mkDerivation (let
          ename = "extended-faces";
          pname = "emacs-${ename}";
          version = "0.1.0";
        in {
          inherit ename pname src version;

          nativeBuildInputs = [
            pkgs.emacs
            # Emacs-lisp build tool, https://doublep.github.io/eldev/
            pkgs.emacsPackages.eldev
          ];

          postUnpack = relocateHome;

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
            inherit src;

            name = "eldev-doctor";

            nativeBuildInputs = [
              pkgs.emacs
              # Emacs-lisp build tool, https://doublep.github.io/eldev/
              pkgs.emacsPackages.eldev
            ];

            postUnpack = relocateHome;

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
            inherit src;

            name = "eldev-lint";

            nativeBuildInputs = [
              pkgs.emacsPackages.eldev
              (pkgs.emacsWithPackages (epkgs: [
                epkgs.eldev
                epkgs.elisp-lint
                epkgs.relint
              ]))
            ];

            postUnpack = relocateHome;

            postPatch = ''
              {
                echo '(setq package-lint-main-file "extended-faces.el")'
                ## TODO: Remove this line once we make the more significant
                ##       changes required to allow `package-lint` to pass.
                echo "(setq elisp-lint-ignored-validators '(\"package-lint\"))"
              } >> Eldev
            '';

            buildPhase = ''
              runHook preBuild
              eldev lint doc elisp re
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
