{
  description = "Cabal file parser and Hackage compliance tests";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  outputs = { self, nixpkgs }: let
    systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
    each = nixpkgs.lib.genAttrs systems;
    pkgsFor = system: import nixpkgs { inherit system; };
    # The compliance tests compare with this Cabal-syntax release.
    # The pinned Nixpkgs revision does not contain it.
    hpFor = system: (pkgsFor system).haskellPackages.override {
      overrides = self: _: {
        Cabal-syntax = self.callHackageDirect {
          pkg = "Cabal-syntax";
          ver = "3.18.1.0";
          sha256 = "sha256-UN4+xwuRA7gUQrsuCfETQK+plV6yn6pNFEISBquvSHk=";
        } {};
      };
    };
    package = system: let
      pkgs = pkgsFor system;
      hp = hpFor system;
    in hp.mkDerivation {
      pname = "aihc-cabal-syntax";
      version = "2.0.0.0";
      src = pkgs.lib.fileset.toSource {
        root = ./.;
        fileset = pkgs.lib.fileset.unions [ ./src ./test ./aihc-cabal-syntax.cabal ./LICENSE ./README.md ./CHANGELOG.md ];
      };
      isLibrary = true;
      isExecutable = false;
      libraryHaskellDepends = with hp; [ base bytestring containers text megaparsec parser-combinators ];
      testHaskellDepends = with hp; [ base bytestring containers text Cabal-syntax aeson tar directory filepath hedgehog ];
      license = pkgs.lib.licenses.unlicense;
      doCheck = true;
    };
    # GHC also contains Cabal-syntax, and its Cabal library exports those modules.
    # Hide them and select the reference version.
    cabalSyntaxFlag = system:
      "-hide-package Cabal -package Cabal-syntax-${(hpFor system).Cabal-syntax.version}";
    runner = system: let
      pkgs = pkgsFor system;
      compiler = (hpFor system).ghcWithPackages (hp:
        pkgs.lib.filter (dependency: dependency != null)
          (with hp; [ (package system) bytestring containers text Cabal-syntax aeson tar directory filepath ]));
    in pkgs.runCommand "hackage-compliance-test-runner" {
      nativeBuildInputs = [ compiler ];
    } ''
      ghc -O2 -Wall -Werror ${cabalSyntaxFlag system} -i${./test} \
        -odir . -hidir . ${./test}/Hackage.hs -o "$out"
    '';
    benchmark = system: let
      pkgs = pkgsFor system;
      compiler = (hpFor system).ghcWithPackages (hp: [ (package system) hp.tar hp.bytestring hp.Cabal-syntax ]);
    in pkgs.runCommand "hackage-benchmark" { nativeBuildInputs = [ compiler ]; } ''
      ghc -O2 -Wall -Werror ${cabalSyntaxFlag system} -odir . -hidir . ${./test/Benchmark.hs} -o "$out"
    '';
    updateReadme = system: let pkgs = pkgsFor system; in
      pkgs.writeShellApplication {
        name = "update-readme";
        runtimeInputs = [ pkgs.python3 ];
        text = ''
          python ${./tests/hackage/check_baseline.py} ${compliance system} \
            ${compliance system}/summary.json ${./tests/hackage/snapshot.json}
          python ${./tests/hackage/check_stackage.py} ${stackageCompliance system} \
            ${./tests/hackage/stackage.json}
          python ${./scripts/readme.py} ${compliance system}/summary.json \
            --stackage ${stackageCompliance system} \
            --runner ${benchmark system} --index ${corpus system}/index.tar \
            --system ${system} --ghc-version ${(hpFor system).ghc.version} "$@"
        '';
      };
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
    # The pinned nixpkgs revision supplies the Stackage LTS package list.
    stackageCorpus = system: let pkgs = pkgsFor system; in
      pkgs.runCommand "stackage-cabal-corpus" { nativeBuildInputs = [ pkgs.python3 ]; } ''
        python ${./tests/hackage/stackage.py} \
          ${nixpkgs}/pkgs/development/haskell-modules/configuration-hackage2nix/stackage.yaml \
          ${corpus system}/index.tar ${corpus system}/manifest.jsonl "$out"
      '';
    stackageCompliance = system: let pkgs = pkgsFor system; in
      pkgs.runCommand "stackage-compliance" {} ''
        ${runner system} ${stackageCorpus system}/index.tar "$out"
        cp ${stackageCorpus system}/summary.json "$out/snapshot.json"
      '';
  in {
    packages = each (system: {
      default = package system;
      update-readme = updateReadme system;
      hackage-corpus = corpus system;
      hackage-compliance = compliance system;
      stackage-corpus = stackageCorpus system;
      stackage-compliance = stackageCompliance system;
    });
    checks = each (system: let pkgs = pkgsFor system; in {
      readme = pkgs.runCommand "check-readme" { nativeBuildInputs = [ pkgs.python3 ]; } ''
        cp ${./README.md} README.md
        mkdir -p tests/hackage
        cp ${./tests/hackage/benchmark.json} tests/hackage/benchmark.json
        cp ${./tests/hackage/results.json} tests/hackage/results.json
        python ${./scripts/readme.py} ${compliance system}/summary.json \
          --stackage ${stackageCompliance system} --check
        touch "$out"
      '';
      benchmark-runner = pkgs.runCommand "check-benchmark-runner" { nativeBuildInputs = [ pkgs.python3 ]; } ''
        python ${./tests/hackage/check_benchmark.py} ${benchmark system}
        touch "$out"
      '';
      stats-update = pkgs.runCommand "check-stats-update" { nativeBuildInputs = [ pkgs.python3 ]; } ''
        export PYTHONDONTWRITEBYTECODE=1
        python ${./tests/hackage/check_stats_update.py} ${./scripts/readme.py}
        touch "$out"
      '';
      workflows = pkgs.runCommand "check-workflows" { nativeBuildInputs = [ pkgs.actionlint ]; } ''
        actionlint ${./.github/workflows/ci.yml} ${./.github/workflows/weekly-stats.yml}
        touch "$out"
      '';
      parser = package system;
      stackage-compliance = pkgs.runCommand "check-stackage-compliance" {
        nativeBuildInputs = [ pkgs.python3 ];
      } ''
        python ${./tests/hackage/check_stackage.py} ${stackageCompliance system} ${./tests/hackage/stackage.json}
        touch "$out"
      '';
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
      default = (hpFor system).shellFor {
        packages = _: [ (package system) ];
        nativeBuildInputs = [ pkgs.cabal-install pkgs.python3 pkgs.curl ];
        shellHook = "export GHC_ENVIRONMENT=-";
      };
    });
  };
}
