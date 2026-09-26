{
  description = "Cabal file parser and Hackage compliance tests";
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
      src = pkgs.lib.fileset.toSource {
        root = ./.;
        fileset = pkgs.lib.fileset.unions [ ./src ./test ./aihc-cabal-syntax.cabal ./LICENSE ./README.md ];
      };
      isLibrary = true;
      isExecutable = false;
      libraryHaskellDepends = with hp; [ base bytestring containers text megaparsec parser-combinators ];
      testHaskellDepends = with hp; [ base bytestring containers text Cabal-syntax aeson tar directory filepath ];
      license = pkgs.lib.licenses.unlicense;
      doCheck = true;
    };
    runner = system: let
      pkgs = pkgsFor system;
      compiler = pkgs.haskellPackages.ghcWithPackages (hp:
        pkgs.lib.filter (dependency: dependency != null)
          (with hp; [ (package system) bytestring containers text Cabal-syntax aeson tar directory filepath ]));
    in pkgs.runCommand "hackage-compliance-test-runner" {
      nativeBuildInputs = [ compiler ];
    } ''
      ghc -O2 -Wall -Werror -i${./test} \
        -odir . -hidir . ${./test}/Hackage.hs -o "$out"
    '';
    corpus = system: let
      pkgs = pkgsFor system;
      snapshot = builtins.fromJSON (builtins.readFile ./tests/hackage/snapshot.json);
      index = pkgs.fetchurl {
        name = "hackage-index-prefix.tar";
        inherit (snapshot) url hash;
        downloadToTemp = true;
        nativeBuildInputs = [ pkgs.python3 ];
        postFetch = ''
          python ${./tests/hackage/prefix.py} "$downloadedFile" "$out" ${toString snapshot.prefixBytes}
        '';
      };
    in pkgs.runCommand "hackage-cabal-corpus" {
      nativeBuildInputs = [ pkgs.python3 ];
    } ''
      mkdir -p "$out"
      cat ${index} > "$out/index.tar"
      python -c 'import sys; open(sys.argv[1], "ab").write(bytes(1024))' "$out/index.tar"
      python ${./tests/hackage/corpus.py} scan "$out/index.tar" \
        "$out/manifest.jsonl" "$out/summary.json" \
        --expect ${./tests/hackage/snapshot.json}
    '';
    compliance = system: let pkgs = pkgsFor system; in
      pkgs.runCommand "hackage-compliance" {} ''
        ${runner system} ${corpus system}/index.tar "$out"
        cp ${./tests/hackage/snapshot.json} "$out/snapshot.json"
      '';
  in {
    packages = each (system: {
      default = package system;
      hackage-corpus = corpus system;
      hackage-compliance = compliance system;
    });
    checks = each (system: let pkgs = pkgsFor system; in {
      parser = package system;
      hackage-compliance = pkgs.runCommand "check-hackage-baseline" {
        nativeBuildInputs = [ pkgs.python3 ];
      } ''
        python ${./tests/hackage/check_baseline.py} ${compliance system} \
          ${./tests/hackage/baseline.json} ${./tests/hackage/snapshot.json}
        touch "$out"
      '';
      compliance-runner = pkgs.runCommand "check-compliance-runner" {
        nativeBuildInputs = [ pkgs.python3 ];
      } ''
        python ${./tests/hackage/check_runner.py} ${runner system}
        test ! -e ${package system}/bin/hackage-compliance
        touch "$out"
      '';
      corpus-reader = pkgs.runCommand "check-corpus-reader" {
        nativeBuildInputs = [ pkgs.python3 ];
      } ''
        export PYTHONDONTWRITEBYTECODE=1
        python -m unittest discover -s ${./tests/hackage} -v
        touch "$out"
      '';
    });
    devShells = each (system: let pkgs = pkgsFor system; in {
      default = pkgs.haskellPackages.shellFor {
        packages = _: [ (package system) ];
        nativeBuildInputs = [ pkgs.cabal-install pkgs.python3 pkgs.curl ];
        shellHook = "export GHC_ENVIRONMENT=-";
      };
    });
  };
}
