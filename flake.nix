{
  description = "Http library for Lean";

  inputs = {
    lean = {
      url = "github:leanprover/lean4";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    utils = {
      url = "github:yatima-inc/nix-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # A lean dependency
    Socket-lean = {
      url = "github:yatima-inc/Socket.lean";
      # Compile dependencies with the same lean version
      inputs.lean.follows = "lean";
    };
    OpenSSL-lean = {
      url = "github:yatima-inc/OpenSSL.lean/acs/work";
      # Compile dependencies with the same lean version
      inputs.lean.follows = "lean";
    };
  };

  outputs = { self, lean, utils, nixpkgs, Socket-lean, OpenSSL-lean }:
    let
      supportedSystems = [
        "aarch64-linux"
        "aarch64-darwin"
        "i686-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      inherit (utils) lib;
    in
    lib.eachSystem supportedSystems (system:
      let
        leanPkgs = lean.packages.${system};
        pkgs = nixpkgs.legacyPackages.${system};
        name = "Http";  # must match the name of the top-level .lean file
        project = leanPkgs.buildLeanPackage {
          inherit name;
          deps = [ OpenSSL-lean.project.${system} Socket-lean.project.${system} ];
          # Where the lean files are located
          src = ./src;
        };
        Cli = leanPkgs.buildLeanPackage {
          name = "Http.Cli";
          deps = [ project ];
          # Where the lean files are located
          src = ./src;
        };
        test = leanPkgs.buildLeanPackage {
          name = "Tests";
          deps = [ project ];
          # Where the lean files are located
          src = ./test;
        };
        joinDepsDerivations = getSubDrv:
          pkgs.lib.concatStringsSep ":" (map (d: "${getSubDrv d}") (project.allExternalDeps));
      in
      {
        inherit project;
        packages = project // {
          ${name} = project.executable;
          inherit Cli;
          test = test.executable;
        };

        checks.test = test.executable;

        defaultPackage = self.packages.${system}.Cli.executable;
        devShell = pkgs.mkShell {
          inputsFrom = [ project.executable ];
          buildInputs = with pkgs; [
            leanPkgs.lean-dev
          ];
          LEAN_PATH = "./src:./test:" + joinDepsDerivations (d: d.modRoot);
          LEAN_SRC_PATH = "./src:./test:" + joinDepsDerivations (d: d.src);
        };
      });
}
