{ pkgs ? import <nixpkgs> {}
, solver
, input ? ./input.txt }:
let
  solver-id = baseNameOf solver;
  for = solver : pkgs.haskellPackages.callCabal2nix solver-id (pkgs.stdenv.mkDerivation rec {
  name = "${solver-id}-src";
  cabal = ../codejam.cabal;
  srcs = [ ../LICENSE ../GCJ.hs ];
  phases = ["unpackPhase"];
  unpackPhase = ''
    set -Eux
    mkdir $out
    cat ${cabal} | sed -e 's/Main.hs/${solver-id}/' > $out/codejam.cabal
    cp ${solver} $out/${solver-id}
    ${pkgs.lib.concatStrings (map (f: "cp ${f} $out/${baseNameOf f}\n") srcs)}
    set +Eux
  '';
  }) {};
  run = solver : input : pkgs.stdenv.mkDerivation {
  name = "${solver-id}-on-${baseNameOf input}";
  src = input;
  phases = ["buildPhase"];
  buildPhase = ''
    set -Eux
    ${for solver}/bin/codejam < ${input} | tee $out
    set +Eux
  '';
  };
  code = solver : pkgs.runCommandCC "${solver-id}-merged" {} ''
    sed -e "/import GCJ/{r ${../GCJ.hs}" -e "a-- END GCJ general stuff" -e "d}" ${solver} | sed -e 's/module GCJ.*/-- BEG GCJ general stuff/g' > $out
  '';
in {
  run = run solver input;
  solver = for solver;
  code = { test = for (code solver);
           code = code solver; };
}
