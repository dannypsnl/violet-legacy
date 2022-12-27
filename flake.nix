{
  description = "violet programming language";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    idris2-pkgs.url = "github:claymager/idris2-pkgs";
    nixpkgs.follows = "idris2-pkgs/nixpkgs";
  };

  outputs = { self, nixpkgs, idris2-pkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ idris2-pkgs.overlay ]; };
        inherit (pkgs.idris2-pkgs._builders) idrisPackage devEnv;
        violet = idrisPackage ./. { };
        runTests = idrisPackage ./test { extraPkgs.violet = violet; };
      in
      {
        defaultPackage = violet;
        packages = { inherit violet runTests; };
        devShell = pkgs.mkShell {
          buildInputs = [ (devEnv violet) ];
        };
      }
    );
}
