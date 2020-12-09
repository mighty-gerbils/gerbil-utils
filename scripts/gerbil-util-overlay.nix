pkgs: super:
  let inherit (super) lib fetchFromGitHub;
      inherit (lib) POP; in {

# This package has bitrotten. Disabled until this PR is merged:
#    https://github.com/drewc/gerbil-local-time/pull/1
/*
  gerbil-support = POP.extendPop super.gerbil-support (gerbil-support: super:
    let defaults = gerbil-support.prePackage-defaults; in {
    prePackages-unstable = POP.kxPop super.prePackages-unstable {
      gerbil-local-time = POP.extendPop defaults (self: super: rec {
        pname = "gerbil-local-time";
        version = "unstable-2018-01-30";
        git-version = "576cb97";
        softwareName = "local-time";
        package = "local-time";
        gerbilInputs = [ self.gerbilPackages.gerbil-utils ];
        version-path = "";
        pre-src = {
          fun = fetchFromGitHub;
          owner = "drewc";
          repo = "gerbil-local-time";
          rev = "576cb97aac936be92a8a2d6ca31ab8673f264935";
          sha256 = "1xyslg5maz60wxrc4jqndx8czfi8b2b9f0n0rsmrx19b6w8giddf";
        };
        meta = {
          description = "A gerbil package that implements \"The Long, Painful History of Time\"";
          homepage    = "https://github.com/drewc/gerbil-local-time";
          license     = lib.licenses.publicDomain;
          platforms   = lib.platforms.unix;
          maintainers = with lib.maintainers; [ fare ];
        };});};});
*/
}
