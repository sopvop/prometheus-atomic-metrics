{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
  };
  outputs = {self, nixpkgs}:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    pkg = pkgs.haskellPackages.callPackage (import ./default.nix) {};
  in {
    devShells."${system}".default = pkg.env;
  };

}
