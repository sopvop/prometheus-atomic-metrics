{ mkDerivation, base, ghc-prim, lib, primitive, prometheus-client
, vector
}:
mkDerivation {
  pname = "prometheus-historgram-vector";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base ghc-prim primitive prometheus-client vector
  ];
  testHaskellDepends = [ base ];
  license = lib.licenses.asl20;
}
