let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          matrix-bot = haskellPackagesNew.callCabal2nix "matrix-bot" ./. {};
        };
      };
    };
  };

  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs-channels";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { inherit config; };

  # We need this "mess" in order to get a minimal base image containing /etc/services, /etc/protocols
  # and maybe even certificates. http(s) is hard.
  # See https://github.com/NixOS/nixpkgs/issues/18038
  minimalDocker = {
    imports = [ "${src}/nixos/modules/profiles/minimal.nix" ];
    boot.isContainer = true;
  };

  eval = import "${src}/nixos/lib/eval-config.nix" {
    modules = [
      minimalDocker
    ];
  };

  system = eval.config.system;

in
  pkgs.dockerTools.buildImage {
    name = "matrix-bot";
    tag = "latest";
    contents = pkgs.symlinkJoin {
      name = "matrix-bot-contents";
      paths = [
        (pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.matrix-bot)
	system.build.etc
	system.path
      ];
    };
    config = {
      Cmd = [ "matrix-bot-exe" ];
      Volumes = {
        "/etc/matrix-bot" = {};
      };
    };
  }
