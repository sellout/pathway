{
  config,
  flaky,
  lib,
  pkgs,
  self,
  supportedSystems,
  ...
}: {
  project = {
    name = "pathway";
    summary = "Type-safe system-independent file path library";
  };

  imports = [./hlint.nix];

  ## dependency management
  services.renovate.enable = true;

  ## development
  programs = {
    direnv.enable = true;
    # This should default by whether there is a .git file/dir (and whether it’s
    # a file (worktree) or dir determines other things – like where hooks
    # are installed.
    git.enable = true;
  };

  ## formatting
  editorconfig.enable = true;

  programs = {
    treefmt.enable = true;
    vale.enable = true;
  };

  ## CI
  services.garnix.enable = true;
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
      self.lib.githubSystems);
  services.haskell-ci = {
    inherit (self.lib) defaultGhcVersion;
    systems = self.lib.githubSystems;
    ghcVersions = self.lib.nonNixTestedGhcVersions;
    cabalPackages = {
      pathway = "pathway";
      pathway-internal = "internal";
      pathway-path = "path";
      pathway-quickcheck = "quickcheck";
      pathway-system = "system";
    };
    latestGhcVersion = "9.10.1";
  };

  ## publishing
  services.github.enable = true;
  services.github.settings.repository.topics = [];
}
