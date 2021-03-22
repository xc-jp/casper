let hsPkgs = import ./.;
in
hsPkgs.shellFor {
  withHoogle = true;
  tools = {
    cabal = "3.2.0.0";
    hlint = "3.1.5";
    ghcid = "0.8.7";
    haskell-language-server = "latest";
    ghcide = "0.2.0";
    ormolu = "0.1.0.0";
  };
  exactDeps = true;
}
