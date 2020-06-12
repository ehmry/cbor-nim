# Package

version       = "0.6.0"
author        = "Emery Hemingway"
description   = "Concise Binary Object Representation decoder and encoder (RFC7049)."
license       = "MIT"
srcDir        = "src"
installExt    = @["nim"]
bin           = @["bin/cbordiag"]
skipDirs      = @["bin"]


# Dependencies

requires "nim >= 0.20.0"
