with import <nixpkgs> {};
stdenv.mkDerivation rec {
     name = "auction";
     buildInputs = [
                 scala
                 sbt
     ];
}
