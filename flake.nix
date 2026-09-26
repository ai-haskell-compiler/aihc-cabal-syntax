{
  description = "Cabal file parser";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  outputs = { self, nixpkgs }: let
    systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
    each = nixpkgs.lib.genAttrs systems;
    pkgsFor = system: import nixpkgs { inherit system; };
    package = system: let
      pkgs = pkgsFor system;
      hp = pkgs.haskellPackages;
    in hp.mkDerivation {
      pname = "aihc-cabal-syntax";
      version = "0.1.0.0";
      src = pkgs.lib.cleanSource ./.;
      libraryHaskellDepends = with hp; [ base bytestring containers text megaparsec parser-combinators ];
      testHaskellDepends = with hp; [ base bytestring containers text Cabal-syntax ];
      license = pkgs.lib.licenses.unlicense;
      doCheck = true;
    };
  in {
    packages = each (system: { default = package system; });
    checks = each (system: { parser = package system; });
    devShells = each (system: let pkgs = pkgsFor system; in {
      default = pkgs.haskellPackages.shellFor {
        packages = _: [ (package system) ];
        nativeBuildInputs = [ pkgs.cabal-install ];
        shellHook = "export GHC_ENVIRONMENT=-";
      };
    });
  };
}
