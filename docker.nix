let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  colvid-server = import ./release.nix {};

in pkgs.dockerTools.buildImage {
  name = "colvid-server-docker";
  tag = "latest";
  contents = [
    colvid-server
    pkgs.busybox
  ];
  config = {
    Cmd = ["/bin/${colvid-server.pname}"];
  };
}
