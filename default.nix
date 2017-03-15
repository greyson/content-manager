{ mkDerivation, base, bytedump, bytestring, containers, cryptohash
, directory, filepath, mtl, process, stdenv, unix
}:
mkDerivation {
  pname = "content-manager";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytedump bytestring containers cryptohash directory filepath
    mtl process unix
  ];
  description = "A POSIX filesystem-based content management system";
  license = stdenv.lib.licenses.gpl3;
}
