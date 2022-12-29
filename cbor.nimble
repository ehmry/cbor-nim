# Package

version = "20221007"
author        = "Emery Hemingway"
description   = "Concise Binary Object Representation decoder and encoder (RFC8949)."
license       = "Unlicense"
srcDir        = "src"
installExt    = @["nim"]
bin           = @["bin/cbordiag"]
skipDirs      = @["bin"]


# Dependencies

requires "nim >= 0.20.0", "bigints"
