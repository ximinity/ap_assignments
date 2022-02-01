with ((import (fetchTarball {
  name = "nixpkgs-master-2021-12-26";
  url = "https://github.com/nixos/nixpkgs/archive/9a98b3a7094019637fd6eaf451a310f3614b72e4.tar.gz";
  sha256 = "15apglrn5pfhyr6vsynnvmax6nlrwmv0wfqawnrkagki01l4bqck";
}) {}));
let

  sacVCs = {
    version = "1.3.3";
    vname = "MijasCosta";
    changes = "705";
    rev = "1";
    commit = "g41ed2";
  };
  libxcrypt = pkgs.callPackage ./nix/libxcrypt.nix { };
  sacStdLib = callPackage ./nix/stdlib.nix { inherit sacVCs; };
  sac2c = pkgs.callPackage ./nix/sac2c.nix { inherit sacStdLib sacVCs libxcrypt; };

  extensions = (with pkgs.vscode-extensions; [
    ms-vsliveshare.vsliveshare
    ms-python.python
  ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
    name = "cleanLang";
    publisher = "lucas-franceschino";
    version = "0.2.0";
    sha256 = "1i5j24mgnhm4qbzwxqvk4ps7m5knhqfsa4mdx0r7lcbxyp1i4clx";
  }]);
  vscode-with-extensions = pkgs.vscode-with-extensions.override {
    vscodeExtensions = extensions;
  };
in pkgs.mkShell {
  buildInputs = [
    sac2c
    python38
    python38Packages.matplotlib
    vscode-with-extensions
  ];
}
