{ pkgs ? import <nixpkgs> {} }:
let
  for = problem : pkgs.haskellPackages.callCabal2nix (baseNameOf problem) (pkgs.stdenv.mkDerivation rec {
  name = "${baseNameOf problem}-src";
  cabal = ./codejam.cabal;
  srcs = [ ./LICENSE ];
  phases = ["unpackPhase"];
  unpackPhase = ''
    set -Eux
    mkdir $out
    cat ${cabal} | sed -e 's/Main.hs/${baseNameOf problem}/' > $out/codejam.cabal
    cp ${problem} $out/${baseNameOf problem}
    ${pkgs.lib.concatStrings (map (f: "cp ${f} $out/${baseNameOf f}") srcs)}
    set +Eux
  '';
}) {};
in {
  slarbo = for ./slarbo.hs;
}
