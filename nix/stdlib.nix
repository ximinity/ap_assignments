{ lib, fetchurl, stdenv, sacVCs }:

let
  sacVersion = with sacVCs; "${version}-${changes}-${rev}";
  versionedPathSegment = with sacVCs; "sac2c/${version}-${vname}-${changes}-${commit}";
in
stdenv.mkDerivation rec {
  pname = "sacStdLib";
  version = "1.3";
  changes = "137";
  commit = "g1cad5";
  name = "${pname}-${version}-${changes}-${commit}";
  src = fetchurl {
    url = "https://gitlab.sac-home.org/sac-group/sac-packages/-/raw/master/packages/weekly/Linux/${sacVersion}/basic/sac-stdlib-${version}-${changes}-${commit}.tar.gz";
    sha256 = "ecbe18742f95b9d90430c16a056cae177b5a193258d8f16880c10c201193cd50";
  };
  #We get rid of the versioned path segments as in nix versioning is in the prefix
  installPhase = ''
    mkdir $out
    mv usr/local/libexec/${versionedPathSegment} $out/libexec
    mv usr/local/lib/${versionedPathSegment} $out/lib
  '';

  meta = with lib; {
    description = "The standard library for the Single-Assignment C programming language";
    homepage = "http://www.sac-home.org/";
  };
}
