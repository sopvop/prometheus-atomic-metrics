{ mkDerivation, base, bytestring, lib, primitive, prometheus-client
, text, vector
}:
mkDerivation {
  pname = "prometheus-atomic-metrics";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring primitive prometheus-client text vector
  ];
  license = lib.licenses.asl20;
}
