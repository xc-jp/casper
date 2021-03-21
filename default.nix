let
  # haskellNix = import <haskell-nix> {};
  nixpkgs = builtins.fetchTarball
    {
      url =
        "https://github.com/NixOS/nixpkgs/archive/78dc359abf8217da2499a6da0fcf624b139d7ac3.tar.gz";
      sha256 = "0wgfvkwxj8vvy100dccffb6igbqljvhgyxdk8c9gk4k2zlkygz45";
    };
  # haskell.nix master as of 2020-12-14
  # Import using haskell.nix v2 format
  haskell-nix = builtins.fetchTarball {
    url =
      "https://github.com/input-output-hk/haskell.nix/archive/22de1b849a7c4137401acc81376fc722d97aafa8.tar.gz";
    sha256 = "1qm7lki0xq5n5w4dcxxawzwkkww5lr9sv45kp2f1zh9rdc1jmf68";
  };
  haskellNix = import haskell-nix { };
  pkgsArgs = haskellNix.nixpkgsArgs;
  pkgs = import nixpkgs pkgsArgs;
in
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "casper-source";
    src = ./.;
  };
  compiler-nix-name = "ghc883";
}
