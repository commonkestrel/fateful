{ lib, fetchFromGitHub, rustPlatform }:

let 
  pkgs = import <nixpkgs> {};
in
rustPlatform.buildRustPackage rec {
  pname = "fateful";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "commonkestrel";
    repo = pname;
    rev = "e8897cc";
    hash = "sha256-910xg3yq5Ne6XhJ71RVqZW2rAdPciYvIv+Zj5KG86Y0=";
  };

  nativeBuildInputs = with pkgs; [
    pkg-config
    openssl.dev
    openssl
    systemd
  ];

  buildInputs = with pkgs; [
    openssl.dev
    openssl
    systemd
	wayland
	libGL
	libxkbcommon
  ];

  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "fateful_macros-0.1.0" = "sha256-heaUewDYEtEW/uhIWeinSGoXDwKSpR75YlKE1xGSMEk=";
    };
  };

  meta = {
    description = "A command-line utility for working with the f8ful CPU";
    homepage = "https://github.com/commonkestrel/fateful";
    licence = lib.licences.mit;
    maintainers = ["commonkestrel"];
  };
}
