# Package

version = "20231006"
author        = "Emery Hemingway"
description   = "Concise Binary Object Representation decoder and encoder (RFC8949)."
license       = "Unlicense"
srcDir        = "src"
installExt    = @["nim"]
bin           = @["bin/cbordiag"]
skipDirs      = @["bin"]


# Dependencies

requires "https://github.com/ehmry/nim-bigints.git >= 20231006"
