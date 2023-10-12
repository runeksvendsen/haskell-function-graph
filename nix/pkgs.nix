{ pkgs =
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/4ecab3273592f27479a583fb6d975d4aba3486fe.tar.gz";
      sha256 = "10wn0l08j9lgqcw8177nh2ljrnxdrpri7bp0g7nvrsn9rkawvlbf";
    }) {};

  release-21-05 =
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/release-21.05.tar.gz";
      sha256 = "12q00nbd7fb812zchbcnmdg3pw45qhxm74hgpjmshc2dfmgkjh4n";
    }) {};
}
