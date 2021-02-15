{ mkDerivation, aeson, base, record-dot-preprocessor
, record-hasfield, servant-server, sqlite-simple, stdenv, text
, time, wai, warp
}:
mkDerivation {
  pname = "colvid-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base record-dot-preprocessor record-hasfield servant-server
    sqlite-simple text time wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/LuisChDev/colvid-server";
  description = "servidor para proyecto colombia video";
  license = stdenv.lib.licenses.bsd3;
}
