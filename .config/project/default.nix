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
  services.github.settings.branches.main.protection.required_status_checks.contexts =
    lib.mkForce
    ([
        "All Garnix checks"
        "check-bounds"
        "check-licenses"
      ]
      ++ lib.concatMap (sys:
        lib.concatMap (ghc: [
          "build (${ghc}, ${sys})"
          "build (--prefer-oldest, ${ghc}, ${sys})"
        ])
        self.lib.nonNixTestedGhcVersions)
      config.services.haskell-ci.systems);
  services.haskell-ci = {
    inherit (self.lib) defaultGhcVersion;
    ghcVersions = self.lib.nonNixTestedGhcVersions;
    extraDependencyVersions = [
      "megaparsec-9.6.1"
    ];
    cabalPackages = {
      pathway-internal = "internal";
      pathway-quickcheck = "quickcheck";
      pathway = "pathway";
      pathway-path = "path";
      pathway-system = "system";
    };
    latestGhcVersion = "9.10.1";
  };

  ## publishing
  services.github.settings.repository.topics = [];
}
