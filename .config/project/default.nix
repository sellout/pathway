### All available options for this file are listed in
### https://sellout.github.io/project-manager/options.xhtml
{
  config,
  lib,
  self,
  ...
}: {
  project = {
    name = "pathway";
    summary = "Type-safe system-independent file path library";
    ## TODO: Move something like this to Flaky.
    file = let
      copyLicenses = dir: {
        "${dir}/LICENSE".source = ../../LICENSE;
        "${dir}/LICENSE.AGPL-3.0-only".source = ../../LICENSE.AGPL-3.0-only;
        "${dir}/LICENSE.Universal-FOSS-exception-1.0".source =
          ../../LICENSE.Universal-FOSS-exception-1.0;
        "${dir}/LICENSE.commercial".source = ../../LICENSE.commercial;
      };
    in
      copyLicenses "internal"
      // copyLicenses "path"
      // copyLicenses "pathway"
      // copyLicenses "quickcheck"
      // copyLicenses "system";
  };

  imports = [./hlint.nix];

  programs.vale.vocab.${config.project.name}.accept = [
    "Dhall"
    "doctest"
    "hostname"
    "Pathy"
    "reparent"
    "trie"
  ];

  ## CI
  ## FIXME: Shouldn’t need `mkForce` here (or to duplicate the base contexts).
  ##        Need to improve module merging.
  services.github.settings = {
    collaborators = [{username = "Aster89";}];
    branches.main.protection.required_status_checks.contexts =
      lib.mkForce
      ([
          "All Garnix checks"
          "check-bounds"
          "check-licenses"
        ]
        ++ lib.concatMap (sys:
          lib.concatMap (ghc:
            ## Don’t add `exclude`d matrix entries to the required list
            ##
            ## TODO: Make this less manual (like the `include` component).
              if
                ghc
                == "8.10.1"
                && builtins.elem sys ["macos-15-intel" "windows-2025"]
                || ghc == "9.2.1" && builtins.elem sys ["macos-15" "ubuntu-24.04-arm"]
                || ghc == "9.4.1" && builtins.elem sys ["macos-15" "macos-15-intel" "ubuntu-24.04" "ubuntu-24.04-arm" "windows-2025"]
              then []
              else [
                "build (${ghc}, ${sys})"
                "build (--prefer-oldest, ${ghc}, ${sys})"
              ])
          self.lib.nonNixTestedGhcVersions)
        config.services.haskell-ci.systems
        ## Add `include`d matrix entries to the required list.
        ++ map (
          entry:
            if entry.bounds == ""
            then "build (${entry.ghc}, ${entry.os})"
            else "build (${entry.bounds}, ${entry.ghc}, ${entry.os})"
        )
        config.services.haskell-ci.include);
  };
  services.haskell-ci = {
    inherit (self.lib) defaultGhcVersion;
    ghcVersions = self.lib.nonNixTestedGhcVersions;
    cabalPackages = {
      pathway-internal = "internal";
      pathway-quickcheck = "quickcheck";
      pathway = "pathway";
      pathway-path = "path";
      pathway-system = "system";
    };
    exclude =
      [
        ## TODO: “can't load framework: Security (not found)”
        ##       Maybe this is fixable with some dependency constraint.
        ##       E.g., doctest 0.16.3 vs 0.24.3.
        {
          ghc = "8.10.1";
          os = "macos-15-intel";
        }
        ## TODO:  “Failed to build pathway-quickcheck-0.0.1.0. […] terminated with exit code 11”
        {
          ghc = "8.10.1";
          os = "windows-2025";
        }
        ## TODO: “Failed to build clock-0.7.1. […] 'ffitarget_arm64.h' file not found”
        ##       I wonder if this could have the same cause as the Ubuntu one
        ##       below – an issue with NUMA on ARM around this version.
        {
          ghc = "9.2.1";
          os = "macos-15";
        }
        ## TODO:  “/usr/bin/ld: cannot find -lnuma: No such file or directory”
        {
          ghc = "9.2.1";
          os = "ubuntu-24.04-arm";
        }
        ## TODO: “Failed to build ghc-paths-0.1.0.12. […] segfaulted”
        ##       I wonder if this could have the same cause as the Ubuntu one
        ##       above – an issue with NUMA on ARM around this version.
        {
          ghc = "9.4.1";
          os = "macos-15";
        }
      ]
      ## TODO: “Failed to build Cabal-syntax-3.14.2.0.”
      ++ map (os: {
        inherit os;
        bounds = "";
        ghc = "9.4.1";
      }) ["macos-15-intel" "ubuntu-24.04" "ubuntu-24.04-arm"]
      ++ [
        ## TODO: “Failed to build Cabal-syntax-3.16.1.0”
        {
          ghc = "9.4.1";
          os = "windows-2025";
        }
      ];
    ## These just try to build with a newer GHC from the same major version.
    ##
    ## TODO: Currently this just picks the _newest_ version instead of the
    ##       oldest, but there may be a middle ground for some of them.
    include =
      map (bounds: {
        inherit bounds;
        ghc = "8.10.7";
        os = "macos-15-intel";
      }) ["--prefer-oldest" ""]
      ++ map (bounds: {
        inherit bounds;
        ghc = "8.10.7";
        os = "windows-2025";
      }) ["--prefer-oldest" ""]
      ++ lib.concatMap (ghc:
        map (bounds: {
          inherit bounds ghc;
          os = "macos-15";
        }) ["--prefer-oldest" ""]) ["9.2.8" "9.4.8"]
      ++ map (bounds: {
        inherit bounds;
        ghc = "9.2.8";
        os = "ubuntu-24.04-arm";
      }) ["--prefer-oldest" ""]
      ++ map (bounds: {
        inherit bounds;
        ghc = "9.4.8";
        os = "windows-2025";
      }) ["--prefer-oldest" ""]
      ++ map (os: {
        inherit os;
        bounds = "";
        ghc = "9.4.8";
      }) ["macos-15-intel" "ubuntu-24.04" "ubuntu-24.04-arm"];
    ## These are versions that we don’t build against, but that we want the
    ## Haskell packages to support anyway. Mostly, this is for local packages
    ## where `--perfer-oldest` and `--allow-newer` have no effect, so reasonable
    ## bounds need to be managed manually.
    ##
    ## These also currently need to be restricted so that they work for all
    ## packages that have them as dependencies. E.g., most packages require
    ## `yaya ^>= 0.5.0`, but `yaya-unsafe` requires `yaya ^>= 0.5.1`, so this
    ## must specify `yaya-0.5.1.0`, not `yaya-0.5.0.0`.
    extraDependencyVersions = [
      "QuickCheck-2.15.0.1" # Used by Nixpkgs 25.11’s haskellPackages
    ];
    latestGhcVersion = "9.10.1";
  };

  ## publishing
  services.github.settings.repository.topics = ["filesystem" "path" "trie"];
}
