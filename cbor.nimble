# Package

version       = "0.2.0"
author        = "Emery Hemingway"
description   = "Concise Binary Object Representation decoder and encoder (RFC7049)."
license       = "MIT"

# Dependencies

requires "nim >= 0.17.2"

skipDirs = @["test"]

task test, "Check implementation against published test vectors":
  exec "nim c  --path:. -r test/test"
