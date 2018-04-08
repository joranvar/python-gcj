{ pkgs ? import <nixpkgs> {}
, solver
, input ? ./input.txt }:
let
  for = solver : pkgs.haskellPackages.callCabal2nix (baseNameOf solver) (pkgs.stdenv.mkDerivation rec {
  name = "${baseNameOf solver}-src";
  cabal = ./codejam.cabal;
  srcs = [ ./LICENSE ];
  phases = ["unpackPhase"];
  unpackPhase = ''
    set -Eux
    mkdir $out
    cat ${cabal} | sed -e 's/Main.hs/${baseNameOf solver}/' > $out/codejam.cabal
    cp ${solver} $out/${baseNameOf solver}
    ${pkgs.lib.concatStrings (map (f: "cp ${f} $out/${baseNameOf f}") srcs)}
    set +Eux
  '';
  }) {};
  run = solver : input : pkgs.stdenv.mkDerivation {
  name = "${baseNameOf solver}-on-${baseNameOf input}";
  src = input;
  phases = ["buildPhase"];
  buildPhase = ''
    set -Eux
    ${for solver}/bin/codejam < ${input} | tee $out
    set +Eux
  '';
  };
in {
  run = run solver input;
  solver = for solver;
}
