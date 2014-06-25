{ haskellPackages ? (import /home/emperor/nixpkgs {}).haskellPackages }:
let
  inherit (haskellPackages) cabal cabalInstall_1_16_0_2
    # How to break a line in inherit
    text mtl transformers websockets; 


in cabal.mkDerivation (self: {
  pname = "project-name";
  version = "1.0.0";
  src = ./.;
  buildDepends = [
    # As imported above
    text mtl transformers
    websockets
    
  ];
  buildTools = [ cabalInstall_1_16_0_2];
  enableSplitObjs = false;
})