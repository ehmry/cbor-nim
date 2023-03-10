# SPDX-License-Identifier: MIT

import
  cbor, cbor / jsonhooks

import
  std / [base64, json, jsonutils, random, tables, strutils, times, unittest]

const
  vectors = readFile "tests/appendix_a.json"
let js = parseJson vectors
suite "decode":
  for v in js.items:
    if v.hasKey "decoded":
      let control = $v["decoded"]
      test control:
        let
          controlCbor = base64.decode v["cbor"].getStr
          c = parseCbor controlCbor
          js = c.toJson
        if js.isNil:
          fail()
        else:
          check(control == $js)
suite "diagnostic":
  for v in js.items:
    if v.hasKey "diagnostic":
      let control = v["diagnostic"].getStr
      test control:
        let
          controlCbor = base64.decode v["cbor"].getStr
          c = parseCbor controlCbor
        check($c == control)
suite "roundtrip":
  for v in js.items:
    if v["roundtrip"].getBool:
      let
        controlB64 = v["cbor"].getStr
        controlCbor = base64.decode controlB64
        c = parseCbor controlCbor
      test $c:
        let testCbor = encode(c)
        if controlCbor != testCbor:
          let testB64 = base64.encode(testCbor)
          check(controlB64 == testB64)
suite "hooks":
  test "DateTime":
    let dt = now()
    var
      bin = encode(dt)
      node = parseCbor(bin)
    check(node.text == $dt)
  test "Time":
    let t = now().toTime
    var
      bin = encode(t)
      node = parseCbor(bin)
    check(node.getInt == t.toUnix)
test "tag":
  var c = toCbor("foo")
  c.tag = 99
  echo c
  check c.tag == 99'u64
test "sorting":
  var map = initCborMap()
  var keys = @[toCbor(10), toCbor(100), toCbor(-1), toCbor("z"), toCbor("aa"),
               toCbor([toCbor(100)]), toCbor([toCbor(-1)]), toCbor(false)]
  shuffle(keys)
  for k in keys:
    map[k] = toCbor(0)
  check not map.isSorted
  sort(map)
  check map.isSorted