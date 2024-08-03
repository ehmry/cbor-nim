# SPDX-License-Identifier: MIT

import
  pkg / bigints, cbor, cbor / bignum

import
  std / [streams, strutils, unittest]

suite "bignum":
  test "18446744073709551616":
    let a = initBigInt("18446744073709551616")
    checkpoint($a)
    var str = newStringStream()
    str.writeCbor(a)
    str.setPosition(0)
    check str.data.toHex == "C249010000000000000000"
  test "18591708106338011146":
    let a = initBigInt("18591708106338011146")
    checkpoint($a)
    var str = newStringStream()
    str.writeCbor(a)
    str.setPosition(0)
    check str.data.toHex == "C24901020304050607080A"
  test "roundtrip":
    let a = initBigInt("-36893488147419103232")
    checkpoint($a)
    var str = newStringStream()
    str.writeCbor(a)
    var parser: CborParser
    block:
      str.setPosition(0)
      parser.open(str)
      parser.next()
      let c = parser.nextNode()
      checkpoint($c)
    block:
      str.setPosition(0)
      parser.open(str)
      parser.next()
      let b = parser.nextBigNum()
      check(a == b)