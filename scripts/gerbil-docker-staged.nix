{ pkgs ? import <nixpkgs> {} }: let usageComment = ''
# Build a docker image with:
    nix-build gerbil-docker-staged.nix && \
    docker load < ./result |& tee docker.log && \
    IMG=$(tail -1 docker.log | cut -d' ' -f3)

# Use it to run Gerbil with:
    docker run -t -i $IMG
    docker run -t -i $IMG env TERM=xterm zsh -i

# If/when satisfied, pick a tag you can write to, and publish with:
    TAG=gerbil/gerbil-nix # or TAG=fahree/gerbil-nix-staged
    docker tag $IMG $TAG && \
    docker push $TAG
'';

inherit (pkgs) lib dockerTools;
inherit (dockerTools) buildImage;

isNotGambit = x: x != pkgs.gambit-unstable && x != pkgs.gambit-support.gambit-bootstrap;

allDeps = with pkgs;[
    # For a basic Nix environment:
    nix bashInteractive cacert coreutils gnutar gzip
    # Basic survivable interaction environment:
    zsh su screen less git openssh rsync passwd
    # Basic development environment for gerbil:
    gcc binutils gnused gnugrep
  ] ++ pkgs.gambit-unstable.buildInputs
    ++ (builtins.filter isNotGambit pkgs.gerbil-unstable.buildInputs);

allGerbils = builtins.filter pkgs.lib.attrsets.isDerivation
               (builtins.attrValues pkgs.gerbilPackages-unstable);

allContents = allDeps ++ allGerbils ++ (with pkgs;[gambit-unstable gerbil-unstable]);

buildImageWithNixDb = (
    allContents:
    args@{ contents ? null, extraCommands ? "", ... }: (
      buildImage (
        args // {
          extraCommands = ''
            echo "Generating the nix database..."
            echo "Warning: only the database of the deepest Nix layer is loaded."
            echo "         If you want to use nix commands in the container, it would"
            echo "         be better to only have one layer that contains a nix store."

            export NIX_REMOTE=local?root=$PWD
            # A user is required by nix
            # https://github.com/NixOS/nix/blob/9348f9291e5d9e4ba3c4347ea1b235640f54fd79/src/libutil/util.cc#L478
            export USER=nobody
            ${pkgs.nix}/bin/nix-store --load-db < ${pkgs.closureInfo {rootPaths = allContents;}}/registration

            mkdir -p nix/var/nix/gcroots/docker/ ;
            ls -l nix/var/nix/gcroots/docker/ ;
            ln -s ${lib.concatStringsSep " " (lib.unique allContents)} nix/var/nix/gcroots/docker/

            mkdir -p usr/bin
            ln -s ../../bin/env usr/bin
          '' + extraCommands;
        }
      )
    )
  );

  users = {

    root = {
      uid = 0;
      shell = "/bin/bash";
      home = "/root";
      gid = 0;
    };

    user = {
      uid = 1000;
      shell = "/bin/bash";
      home = "/home";
      gid = 1000;
    };

  } // lib.listToAttrs (
    map (n: { name = "nixbld${toString n}"; value = {
      uid = 30000 + n;
      gid = 30000;
      groups = [ "nixbld" ];
      description = "Nix build user ${toString n}";
    }; }) (lib.lists.range 1 1));

  groups = {
    root.gid = 0;
    user.gid = 1000;
    nixbld.gid = 30000;
  };

  userToPasswd = k:
    { uid, gid ? 65534, home ? "/var/empty"
    , description ? ""
    , shell ? "${pkgs.coreutils}/bin/false", groups ? [ ] }:
    "${k}:x:${toString uid}:${toString gid}:${description}:${home}:${shell}";
  userToShadow = k: { ... }: "${k}:!:1::::::";
  userToUseradd = k:
    { uid, gid ? 65534, groups ? [ ], ... }:
    "useradd --uid=${toString uid} --gid=${toString gid} ${
      lib.optionalString (groups != [ ]) "-G ${lib.concatStringsSep "," groups}"
    } ${k}";
  passwdContents = lib.concatStringsSep "\n"
    (lib.attrValues (lib.mapAttrs userToPasswd users));
  shadowContents = lib.concatStringsSep "\n"
    (lib.attrValues (lib.mapAttrs userToShadow users));
  createAllUsers = lib.concatStringsSep "\n"
    (lib.attrValues (lib.mapAttrs userToUseradd users));

  groupToGroup = k: { gid }: "${k}:x:${toString gid}:";
  groupToGroupadd = k: { gid }: "groupadd --gid=${toString gid} ${k}";
  createAllGroups = lib.concatStringsSep "\n"
    (lib.attrValues (lib.mapAttrs groupToGroupadd groups));
  groupContents = lib.concatStringsSep "\n"
    (lib.attrValues (lib.mapAttrs groupToGroup groups));

  passwd = pkgs.runCommand "passwd" {
    inherit passwdContents groupContents shadowContents;
    passAsFile = [ "passwdContents" "groupContents" "shadowContents" ];
    allowSubstitutes = false;
    preferLocalBuild = true;
  } ''
    mkdir -p $out/etc

    cat $passwdContentsPath > $out/etc/passwd
    echo "" >> $out/etc/passwd

    cat $groupContentsPath > $out/etc/group
    echo "" >> $out/etc/group

    cat $shadowContentsPath > $out/etc/shadow
    echo "" >> $out/etc/shadow
  '';

#  entrypoint = pkgs.writeScript "entrypoint.sh" ''
#    #!${pkgs.stdenv.shell}
#    #set -e
#    # allow the container to be started with `--user`
#    if [ "$1" = "fahree/gerbil-nix" -a "$(${pkgs.coreutils}/bin/id -u)" = "0" ]; then
#      chown -R user /home
#      exec ${pkgs.su}/bin/su gerbil "$BASH_SOURCE" "$@"
#    fi
#    exec "$@"
#  '';

depsImage = buildImage {
  name = "fahree/gerbil-nix-deps";
  tag = "latest";
  contents = allDeps;
};
gambitImage = buildImage {
  name = "fahree/gerbil-nix-gambit";
  tag = "latest";
  fromImage = depsImage;
  contents = pkgs.gambit-unstable;
};
gerbilImage = buildImage {
  name = "fahree/gerbil-nix-gerbil";
  tag = "latest";
  fromImage = gambitImage;
  contents = pkgs.gerbil-unstable;
};
allImage = buildImage {
  name = "fahree/gerbil-nix-all";
  tag = "latest";
  fromImage = gerbilImage;
  contents = allGerbils;
}; in
buildImageWithNixDb allContents {
  name = "fahree/gerbil-nix";
  tag = "latest";
  fromImage = allImage;
  contents = [];
  extraCommands = ''
    # The user may have to chown / chmod these in a future Dockerfile pass
    # and/or in the entrypoint
    mkdir home
    echo "#" > home/.zshrc
  '';
  config = {
    Cmd = [ "gxi" ];
#    Entrypoint = [ entrypoint ];
    Env = [
      "SSL_CERT_FILE=${pkgs.cacert.out}/etc/ssl/certs/ca-bundle.crt"
      "USER=root"
      "PATH=/run/wrappers/bin:/home/.nix-profile/bin:/home/.nix-profile/sbin:/.nix-profile/bin:/.nix-profile/sbin:/nix/var/nix/profiles/default/bin:/nix/var/nix/profiles/default/sbin:/run/current-system/sw/bin:/run/current-system/sw/sbin:/bin:/sbin:/usr/bin:/usr/sbin"
      "GIT_SSL_CAINFO=${pkgs.cacert.out}/etc/ssl/certs/ca-bundle.crt"
      "NIX_SSL_CERT_FILE==${pkgs.cacert.out}/etc/ssl/certs/ca-bundle.crt"
      "NIX_PATH=nixpkgs=${pkgs.path}"
    ];
#    ExposedPorts = {
#      "7777/tcp" = {};
#    };
#    WorkingDir = "/home";
#    Volumes = {
#      "/home" = {};
#    };
  };
}

## Note that I originally tried to use a Dockerfile as follows,
## but the process is *way* too slow, as in addition to the emulation slowdown,
## it fails to take advantage of the host's nix store so downloads and rebuilds
## everything from scratch at any slight change. Worse, it requires a static
## link to a fixed nixpkgs. Worst: unversioned, it confuses the Docker cache.
#FROM lnl7/nix:2.3.3
#ENV NIXPKGS=http://github.com/fare-patches/nixpkgs/archive/fare.tar.gz
#ENV GAMBIT_PKG=gambit-unstable
#ENV GERBIL_PKG=gerbil-unstable
#RUN nix-env -iA nixos.git nixos.gnugrep nixos.gnused nixos.gnutar nixos.gzip
#RUN nix-shell --pure --run echo --attr $GAMBIT_PKG $NIXPKGS
#RUN nix-env -f $NIXPKGS -iA $GAMBIT_PKG
#RUN nix-shell --pure --run echo --attr $GERBIL_PKG $NIXPKGS
#RUN nix-env -f $NIXPKGS -iA $GERBIL_PKG

# nix-build -E 'with import <nixpkgs>{}; dockerTools.buildImage { name = "name"; contents = [ gerbil-unstable gambit-unstable gcc bash coreutils zsh openssh su screen nix ] ++ pkgs.gerbil-unstable.buildInputs; }' --option builders ''
# nix why-depends ./result /nix/store/xdmdlw4m1ad99xm6y5wrmx655iadfza7-systemd-243
#  https://github.com/NixOS/docker/blob/master/Dockerfile
#  https://github.com/input-output-hk/cardano-explorer/blob/master/docker/default.nix#L312-L325
#  http://lethalman.blogspot.com/2016/04/cheap-docker-images-with-nix_15.html

# TODO: it looks like /bin/gxi and /nix/store/*-gerbil-unstable-*/bin/gxi are different files.
# Is everything duplicated between / and /nix/store ? Is tar called in a way that fails to preserve
# some symlinks or hardlinks? Fix that.
