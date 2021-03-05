let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  colvid-server = import ./. {};

in pkgs.dockerTools.buildImage {
  name = "colvid-server-docker";
  tag = "latest";
  contents = [
    colvid-server
    pkgs.busybox
  ];

  runAsRoot = ''
    mkdir /var
    mkdir /var/assets
    mkdir /var/db
  '';

  config = {
    Cmd = ["/bin/${colvid-server.pname}"];
  };
}
