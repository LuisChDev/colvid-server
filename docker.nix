let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  colvid-server = import ./. {};

  aws_key = builtins.getEnv "AWS_ACCESS_KEY_ID";
  aws_secret = builtins.getEnv "AWS_SECRET_ACCESS_KEY";
  name = builtins.getEnv "HEROKU_APP";

in pkgs.dockerTools.buildImage {
  inherit name;
  tag = "latest";
  contents = [
    colvid-server
    pkgs.busybox
  ];

  runAsRoot = ''
    export AWS_ACCESS_KEY_ID=${aws_key}
    export AWS_SECRET_ACCESS_KEY=${aws_secret}
  '';

  config = {
    Cmd = ["/bin/${colvid-server.pname}"];
  };
}
