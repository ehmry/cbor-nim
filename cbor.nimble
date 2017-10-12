# Package

version       = "0.1.0"
author        = "Emery Hemingway"
description   = "Concise Binary Object Representation decoder (RFC7049)."
license       = "MIT"

# Dependencies

requires "nim >= 0.17.2"

task test, "Check implementation against standard test vectors":
  exec "nim c  --path:. -r tests/vectors"
