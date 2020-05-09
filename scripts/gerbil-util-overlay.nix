pkgs: super: rec {
  inherit (super) lib gerbil-unstable fetchFromGitHub gambit-support gerbil-support;

  gerbilPackages-unstable = super.gerbilPackages-unstable // {
    gerbil-local-time = gerbil-support.gerbilPackage {
      pname = "gerbil-local-time";
      version = "unstable-2018-01-30";
      git-version = "576cb97";
      package = "local-time";
      gerbil = gerbil-unstable;
      gambit-params = gambit-support.unstable-params;
      gerbilInputs = [ super.gerbilPackages-unstable.gerbil-utils ];
      version-path = "";
      src = fetchFromGitHub {
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
      };
    };
  };
}
