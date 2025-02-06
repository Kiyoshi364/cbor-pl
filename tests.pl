:- module(cbor_tests, [
  run_tests/0, run_tests/1, run_tests/2
]).

:- use_module(cbor, [
  cbor/1,
  cbor_item//1,
  cbor_item//2
]).

:- use_module(library(dcgs), [seq//1, phrase/2, phrase/3]).
:- use_module(library(lists), [
  append/3, foldl/4, maplist/2, maplist/3, length/2
]).
:- use_module(library(iso_ext), [call_cleanup/2]).

clpz:monotonic.

pair_unpair_(K-V, [K, V | L], L).

code_char(X, Y) :- char_code(Y, X).

headerlist_payload_input(Header, Payload, In) :-
  maplist(code_char, Payload, Payload1),
  append(Header, Payload1, In).

nwdet_ok(_:T) :- nwdet(T).
:- discontiguous(nwdet/1).

test_item_encode_10 :-
  Item = unsigned(_, 10),
  findall(Out, cbor_item(Item, Out, []), Answers),
  maplist(maplist(char_code), Answers, Answers2),
  Answers2 == [
    [10],
    [24, 10],
    [25, 0, 10],
    [26, 0, 0, 0, 10],
    [27, 0, 0, 0, 0, 0, 0, 0, 10]
  ],
true.

test_item_decode_500 :-
  headerlist_payload_input("\x19\\x01\\xf4\", "", In),
  phrase(cbor_item(X), In),
  X == unsigned(x2, 500),
  cbor(X),
true.

test_item_encode_500 :-
  Item = unsigned(_, 500),
  findall(Out, cbor_item(Item, Out, []), Answers),
  maplist(maplist(char_code), Answers, Answers2),
  Answers2 == [
    [25, 1, 244],
    [26, 0, 0, 1, 244],
    [27, 0, 0, 0, 0, 0, 0, 1, 244]
  ],
true.

test_item_encode_negative_10 :-
  Item = negative(_, -10),
  findall(Out, cbor_item(Item, Out, []), Answers),
  maplist(maplist(char_code), Answers, Answers2),
  Answers2 == [
    [41],
    [56, 9],
    [57, 0, 9],
    [58, 0, 0, 0, 9],
    [59, 0, 0, 0, 0, 0, 0, 0, 9]
  ],
true.

test_item_decode_negative_500 :-
  headerlist_payload_input("\x39\\x01\\xf3\", "", In),
  phrase(cbor_item(X), In),
  X == negative(x2, -500),
  cbor(X),
true.

test_item_encode_negative_500 :-
  Item = negative(_, -500),
  findall(Out, cbor_item(Item, Out, []), Answers),
  maplist(maplist(char_code), Answers, Answers2),
  Answers2 == [
    [57, 1, 243],
    [58, 0, 0, 1, 243],
    [59, 0, 0, 0, 0, 0, 0, 1, 243]
  ],
true.

test_item_decode_negative_18446744073709551615 :-
  headerlist_payload_input("\x3b\\xff\\xff\\xff\\xff\\xff\\xff\\xff\\xfe\", "", In),
  phrase(cbor_item(X), In),
  X == negative(x8, -18446744073709551615),
  cbor(X),
true.

nwdet(test_item_encode_negative_18446744073709551615).
test_item_encode_negative_18446744073709551615 :-
  Item = negative(x8, -18446744073709551615),
  headerlist_payload_input("\x3b\\xff\\xff\\xff\\xff\\xff\\xff\\xff\\xfe\", "", In),
  phrase(cbor_item(Item), Answer),
  Answer == In,
true.

test_item_decode_5_bytes :-
  Len = 5,
  Payload = [0x00, 0x01, 0x02, 0x03, 0x04],
  length(Payload, Len),
  headerlist_payload_input("\x45\", Payload, In),
  phrase(cbor_item(X), In),
  X == bytes(len(i, Len), Payload),
  cbor(X),
true.

test_item_decode_500_bytes :-
  Len = 500,
  length(Payload, Len),
  maplist(=(0), Payload),
  headerlist_payload_input("\x59\\x01\\xf4\", Payload, In),
  phrase(cbor_item(X), In),
  X == bytes(len(x2, Len), Payload),
  cbor(X),
true.

nwdet(test_item_decode_bytes_indefinite).
test_item_decode_bytes_indefinite :-
  headerlist_payload_input("\x5f\\x44\\xaa\\xbb\\xcc\\xdd\\x43\\xee\\xff\\x99\\xff\", "", In),
  phrase(cbor_item(X), In),
  X == bytes(*, [
    bytes(len(i, 4), [0xaa, 0xbb, 0xcc, 0xdd]),
    bytes(len(i, 3), [0xee, 0xff, 0x99])
  ]),
  cbor(X),
true.

test_item_decode_text_ascii_small :-
  Text = "ascii rules!",
  Len = 12,
  length(Text, Len),
  maplist(char_code, Text, Payload),
  headerlist_payload_input("\x6c\", Payload, In),
  phrase(cbor_item(X), In),
  X == text(len(i, Len), Text),
  cbor(X),
true.

test_item_decode_text_ascii_medium :-
  Text = "ascii text with !@#$%*()_-+=\", 0123456789 and LF\n",
  Len = 49,
  length(Text, Len),
  maplist(char_code, Text, Payload),
  headerlist_payload_input("\x78\\x31\", Payload, In),
  phrase(cbor_item(X), In),
  X == text(len(x1, Len), Text),
  cbor(X),
true.

test_item_decode_text_utf8_small :-
  Text = "çáëõũüæ°„«»",
  Len = 11,
  length(Text, Len),
  Payload = [0xc3, 0xa7, 0xc3, 0xa1, 0xc3, 0xab, 0xc3, 0xb5, 0xc5, 0xa9, 0xc3, 0xbc, 0xc3, 0xa6, 0xc2, 0xb0, 0xe2, 0x80, 0x9e, 0xc2, 0xab, 0xc2, 0xbb],
  PLen = 23,
  length(Payload, PLen),
  headerlist_payload_input("\x77\", Payload, In),
  phrase(cbor_item(X), In),
  X == text(len(i, PLen), Text),
  cbor(X),
true.

test_item_decode_text_utf8_medium :-
  Text = "😀😶‍🌫🫱🏻‍🫲🏿",
  Len = 9,
  length(Text, Len),
  Payload = [0xf0, 0x9f, 0x98, 0x80, 0xf0, 0x9f, 0x98, 0xb6, 0xe2, 0x80, 0x8d, 0xf0, 0x9f, 0x8c, 0xab, 0xf0, 0x9f, 0xab, 0xb1, 0xf0, 0x9f, 0x8f, 0xbb, 0xe2, 0x80, 0x8d, 0xf0, 0x9f, 0xab, 0xb2, 0xf0, 0x9f, 0x8f, 0xbf],
  PLen = 34,
  length(Payload, PLen),
  headerlist_payload_input("\x78\\x22\", Payload, In),
  phrase(cbor_item(X), In),
  X == text(len(x1, PLen), Text),
  cbor(X),
true.

nwdet(test_item_decode_text_indefinite).
test_item_decode_text_indefinite :-
  headerlist_payload_input("\x7f\\x63\\x61\\x62\\x63\\x62\\x30\\x31\\xff\", "", In),
  phrase(cbor_item(X), In),
  X == text(*, [
    text(len(i, 3), "abc"),
    text(len(i, 2), "01")
  ]),
  cbor(X),
true.

test_item_decode_array :-
  Items = [unsigned(i, 10), unsigned(x2, 500), negative(i, -10), negative(x2, -500)],
  Len = 4,
  length(Items, Len),
  once(foldl(cbor_item, Items, Payload, [])),
  append("\x84\", Payload, In),
  phrase(cbor_item(X), In),
  X == array(len(i, Len), Items),
  cbor(X),
true.

test_item_decode_map :-
  Pairs = [unsigned(i, 10)-unsigned(x2, 500), negative(i, -10)-negative(x2, -500)],
  foldl(pair_unpair_, Pairs, Items, []),
  Len = 2,
  length(Pairs, Len),
  once(foldl(cbor_item, Items, Payload, [])),
  append("\xa2\", Payload, In),
  phrase(cbor_item(X), In),
  X == map(len(i, Len), Pairs),
  cbor(X),
true.

nwdet(test_item_decode_indefinite_map_unsigned).
test_item_decode_indefinite_map_unsigned :-
  headerlist_payload_input("\xbf\\x20\\xf5\\x01\\x21\\xff\", "", In),
  phrase(cbor_item(X), In),
  X == map(*, [
    negative(i, -1)-simple(i, 21),
    unsigned(i, 1)-negative(i, -2)
  ]),
  cbor(X),
true.

test_item_decode_tag_small :-
  TagNumber = 10,
  Item = unsigned(x2, 500),
  once(phrase(cbor_item(Item), Payload)),
  append("\xca\", Payload, In),
  phrase(cbor_item(X), In),
  X == tag(i, TagNumber, Item),
  cbor(X),
true.

test_item_decode_tag_big :-
  TagNumber = 500,
  Item = unsigned(x2, 500),
  once(phrase(cbor_item(Item), Payload)),
  append("\xd9\\x01\\xf4\", Payload, In),
  phrase(cbor_item(X), In),
  X == tag(x2, TagNumber, Item),
  cbor(X),
true.

test_item_decode_simple_small :-
  headerlist_payload_input("\xf4\", "", In),
  phrase(cbor_item(X), In),
  X == simple(i, 20),
  cbor(X),
true.

test_item_decode_simple_big :-
  headerlist_payload_input("\xf8\\x78\", "", In),
  phrase(cbor_item(X), In),
  X == simple(x1, 120),
  cbor(X),
true.

% TODO: add tests for floats

test_byte_decode_array :-
  Items = [unsigned(i, 10), unsigned(x2, 500), negative(i, -10), negative(x2, -500)],
  Len = 4,
  length(Items, Len),
  In = [0x84, 0x0a, 0x19, 0x01, 0xf4, 0x29, 0x39, 0x01, 0xf3],
  InLen = 9,
  length(In, InLen),
  phrase(cbor_item(X, [listOf(byte)]), In),
  X == array(len(i, Len), Items),
  cbor(X),
true.

test_char_decode_simple_big :-
  headerlist_payload_input("\xf8\\x78\", "", In),
  phrase(cbor_item(X, [listOf(char)]), In),
  X == simple(x1, 120),
  cbor(X),
true.

same_length([], []).
same_length([_ | As], [_ | Bs]) :- same_length(As, Bs).

custom_bytes_text(Bytes, Text) :-
  same_length(Bytes, Text),
  maplist(char_code, Text, Bytes).

test_custom_bytes_text :-
  Text = "ascii rules!",
  Len = 12,
  length(Text, Len),
  append("\x6c\", Text, In),
  Options = [
    listOf(char), bytes_text(cbor_tests:custom_bytes_text)
  ],
  phrase(cbor_item(X, Options), In),
  X == text(len(i, Len), Text),
  cbor(X),
true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RFC 8949 (Appendix A) Begin %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

meta_test_rfc8949_encode(In, Out) :-
  cbor(Out),
  meta_test_rfc8949_encode(In, Out, XIn), XIn == In.
meta_test_rfc8949_encode(_, Out, XIn) :-
  phrase(cbor_item(Out), XIn).

meta_test_rfc8949_decode(In, Out) :-
  cbor(Out),
  meta_test_rfc8949_decode(In, Out, XOut), XOut == Out.
meta_test_rfc8949_decode(In, _, XOut) :-
  phrase(cbor_item(XOut), In).

test_rfc8949_0_decode :-
  headerlist_payload_input("\x00\", "", In),
  Out = unsigned(i, 0),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_0_encode).
test_rfc8949_0_encode :-
  headerlist_payload_input("\x00\", "", In),
  Out = unsigned(i, 0),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_1_decode :-
  headerlist_payload_input("\x01\", "", In),
  Out = unsigned(i, 1),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_1_encode).
test_rfc8949_1_encode :-
  headerlist_payload_input("\x01\", "", In),
  Out = unsigned(i, 1),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_10_decode :-
  headerlist_payload_input("\x0a\", "", In),
  Out = unsigned(i, 10),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_10_encode).
test_rfc8949_10_encode :-
  headerlist_payload_input("\x0a\", "", In),
  Out = unsigned(i, 10),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_23_decode :-
  headerlist_payload_input("\x17\", "", In),
  Out = unsigned(i, 23),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_23_encode).
test_rfc8949_23_encode :-
  headerlist_payload_input("\x17\", "", In),
  Out = unsigned(i, 23),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_24_decode :-
  headerlist_payload_input("\x18\\x18\", "", In),
  Out = unsigned(x1, 24),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_24_encode).
test_rfc8949_24_encode :-
  headerlist_payload_input("\x18\\x18\", "", In),
  Out = unsigned(x1, 24),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_25_decode :-
  headerlist_payload_input("\x18\\x19\", "", In),
  Out = unsigned(x1, 25),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_25_encode).
test_rfc8949_25_encode :-
  headerlist_payload_input("\x18\\x19\", "", In),
  Out = unsigned(x1, 25),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_100_decode :-
  headerlist_payload_input("\x18\\x64\", "", In),
  Out = unsigned(x1, 100),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_100_encode).
test_rfc8949_100_encode :-
  headerlist_payload_input("\x18\\x64\", "", In),
  Out = unsigned(x1, 100),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_1000_decode :-
  headerlist_payload_input("\x19\\x03\\xe8\", "", In),
  Out = unsigned(x2, 1000),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_1000_encode).
test_rfc8949_1000_encode :-
  headerlist_payload_input("\x19\\x03\\xe8\", "", In),
  Out = unsigned(x2, 1000),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_1000000_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\x1a\\x00\\x0f\\x42\\x40\", "", In),
  BIn = [0x1a, 0x00, 0x0f, 0x42, 0x40],
  maplist(code_char, BIn, In),
  Out = unsigned(x4, 1000000),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_1000000_encode).
test_rfc8949_1000000_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\x1a\\x00\\x0f\\x42\\x40\", "", In),
  BIn = [0x1a, 0x00, 0x0f, 0x42, 0x40],
  maplist(code_char, BIn, In),
  Out = unsigned(x4, 1000000),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_1000000000000_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\x1b\\x00\\x00\\x00\\xe8\\xd4\\xa5\\x10\\x00\", "", In),
  BIn = [0x1b, 0x00, 0x00, 0x00, 0xe8, 0xd4, 0xa5, 0x10, 0x00],
  maplist(code_char, BIn, In),
  Out = unsigned(x8, 1000000000000),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_1000000000000_encode).
test_rfc8949_1000000000000_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\x1b\\x00\\x00\\x00\\xe8\\xd4\\xa5\\x10\\x00\", "", In),
  BIn = [0x1b, 0x00, 0x00, 0x00, 0xe8, 0xd4, 0xa5, 0x10, 0x00],
  maplist(code_char, BIn, In),
  Out = unsigned(x8, 1000000000000),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_18446744073709551615_decode :-
  headerlist_payload_input("\x1b\\xff\\xff\\xff\\xff\\xff\\xff\\xff\\xff\", "", In),
  Out = unsigned(x8, 18446744073709551615),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_18446744073709551615_encode).
test_rfc8949_18446744073709551615_encode :-
  headerlist_payload_input("\x1b\\xff\\xff\\xff\\xff\\xff\\xff\\xff\\xff\", "", In),
  Out = unsigned(x8, 18446744073709551615),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_18446744073709551616_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xc2\\x49\\x01\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\", "", In),
  BIn = [0xc2, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = tag(i, 2,
    bytes(len(i, 9), [
      0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ])
  ),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_18446744073709551616_encode).
test_rfc8949_18446744073709551616_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xc2\\x49\\x01\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\", "", In),
  BIn = [0xc2, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = tag(i, 2,
    bytes(len(i, 9), [
      0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ])
  ),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_negative_18446744073709551616_decode :-
  headerlist_payload_input("\x3b\\xff\\xff\\xff\\xff\\xff\\xff\\xff\\xff\", "", In),
  Out = negative(x8, -18446744073709551616),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_negative_18446744073709551616_encode).
test_rfc8949_negative_18446744073709551616_encode :-
  headerlist_payload_input("\x3b\\xff\\xff\\xff\\xff\\xff\\xff\\xff\\xff\", "", In),
  Out = negative(x8, -18446744073709551616),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_negative_18446744073709551617_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xc3\\x49\\x01\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\", "", In),
  BIn = [0xc3, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = tag(i, 3,
    bytes(len(i, 9), [
      0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ])
  ),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_negative_18446744073709551617_encode).
test_rfc8949_negative_18446744073709551617_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xc3\\x49\\x01\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\", "", In),
  BIn = [0xc3, 0x49, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = tag(i, 3,
    bytes(len(i, 9), [
      0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ])
  ),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_negative_1_decode :-
  headerlist_payload_input("\x20\", "", In),
  Out = negative(i, -1),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_negative_1_encode).
test_rfc8949_negative_1_encode :-
  headerlist_payload_input("\x20\", "", In),
  Out = negative(i, -1),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_negative_10_decode :-
  headerlist_payload_input("\x29\", "", In),
  Out = negative(i, -10),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_negative_10_encode).
test_rfc8949_negative_10_encode :-
  headerlist_payload_input("\x29\", "", In),
  Out = negative(i, -10),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_negative_100_decode :-
  headerlist_payload_input("\x38\\x63\", "", In),
  Out = negative(x1, -100),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_negative_100_encode).
test_rfc8949_negative_100_encode :-
  headerlist_payload_input("\x38\\x63\", "", In),
  Out = negative(x1, -100),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_negative_1000_decode :-
  headerlist_payload_input("\x39\\x03\\xe7\", "", In),
  Out = negative(x2, -1000),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_negative_1000_encode).
test_rfc8949_negative_1000_encode :-
  headerlist_payload_input("\x39\\x03\\xe7\", "", In),
  Out = negative(x2, -1000),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_0d0_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xf9\\x00\\x00\", "", In),
  BIn = [0xf9, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_0d0_encode).
test_rfc8949_float_0d0_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xf9\\x00\\x00\", "", In),
  BIn = [0xf9, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_negative_0d0_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xf9\\x00\\x00\", "", In),
  BIn = [0xf9, 0x80, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0x8000),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_negative_0d0_encode).
test_rfc8949_float_negative_0d0_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xf9\\x00\\x00\", "", In),
  BIn = [0xf9, 0x80, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0x8000),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_1d0_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xf9\\x00\\x00\", "", In),
  BIn = [0xf9, 0x3c, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0x3c00),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_1d0_encode).
test_rfc8949_float_1d0_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xf9\\x00\\x00\", "", In),
  BIn = [0xf9, 0x3c, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0x3c00),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_1d1_decode :-
  headerlist_payload_input("\xfb\\x3f\\xf1\\x99\\x99\\x99\\x99\\x99\\x9a\", "", In),
  Out = float(x8, 0x3ff199999999999a),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_1d1_encode).
test_rfc8949_float_1d1_encode :-
  headerlist_payload_input("\xfb\\x3f\\xf1\\x99\\x99\\x99\\x99\\x99\\x9a\", "", In),
  Out = float(x8, 0x3ff199999999999a),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_1d5_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xf9\\x3e\\x00\", "", In),
  BIn = [0xf9, 0x3e, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0x3e00),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_1d5_encode).
test_rfc8949_float_1d5_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xf9\\x3e\\x00\", "", In),
  BIn = [0xf9, 0x3e, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0x3e00),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_65504d0_decode :-
  headerlist_payload_input("\xf9\\x7b\\xff\", "", In),
  Out = float(x2, 0x7bff),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_65504d0_encode).
test_rfc8949_float_65504d0_encode :-
  headerlist_payload_input("\xf9\\x7b\\xff\", "", In),
  Out = float(x2, 0x7bff),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_100000d0_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xfa\\x47\\xc3\\x50\\x00\", "", In),
  BIn = [0xfa, 0x47, 0xc3, 0x50, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x4, 0x47c35000),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_100000d0_encode).
test_rfc8949_float_100000d0_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xfa\\x47\\xc3\\x50\\x00\", "", In),
  BIn = [0xfa, 0x47, 0xc3, 0x50, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x4, 0x47c35000),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_3d4028234663852886ep38_decode :-
  headerlist_payload_input("\xfa\\x7f\\x7f\\xff\\xff\", "", In),
  Out = float(x4, 0x7f7fffff),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_3d4028234663852886ep38_encode).
test_rfc8949_float_3d4028234663852886ep38_encode :-
  headerlist_payload_input("\xfa\\x7f\\x7f\\xff\\xff\", "", In),
  Out = float(x4, 0x7f7fffff),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_1d0ep300_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xfb\\x7e\\x37\\xe4\\x3c\\x88\\x00\\x75\\x9c\", "", In),
  BIn = [0xfb, 0x7e, 0x37, 0xe4, 0x3c, 0x88, 0x00, 0x75, 0x9c],
  maplist(code_char, BIn, In),
  Out = float(x8, 0x7e37e43c8800759c),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_1d0ep300_encode).
test_rfc8949_float_1d0ep300_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xfb\\x7e\\x37\\xe4\\x3c\\x88\\x00\\x75\\x9c\", "", In),
  BIn = [0xfb, 0x7e, 0x37, 0xe4, 0x3c, 0x88, 0x00, 0x75, 0x9c],
  maplist(code_char, BIn, In),
  Out = float(x8, 0x7e37e43c8800759c),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_5d960464477539063em8_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xf9\\x00\\x01\", "", In),
  BIn = [0xf9, 0x00, 0x01],
  maplist(code_char, BIn, In),
  Out = float(x2, 0x0001),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_5d960464477539063em8_encode).
test_rfc8949_float_5d960464477539063em8_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xf9\\x00\\x01\", "", In),
  BIn = [0xf9, 0x00, 0x01],
  maplist(code_char, BIn, In),
  Out = float(x2, 0x0001),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_0d00006103515625_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xf9\\x04\\x00\", "", In),
  BIn = [0xf9, 0x04, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0x0400),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_0d00006103515625_encode).
test_rfc8949_float_0d00006103515625_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xf9\\x04\\x00\", "", In),
  BIn = [0xf9, 0x04, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0x0400),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_negative_4d0_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xf9\\xc4\\x00\", "", In),
  BIn = [0xf9, 0xc4, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0xc400),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_negative_4d0_encode).
test_rfc8949_float_negative_4d0_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xf9\\xc4\\x00\", "", In),
  BIn = [0xf9, 0xc4, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0xc400),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_negative_4d1_decode :-
  headerlist_payload_input("\xfb\\xc0\\x10\\x66\\x66\\x66\\x66\\x66\\x66\", "", In),
  Out = float(x8, 0xc010666666666666),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_negative_4d1_encode).
test_rfc8949_float_negative_4d1_encode :-
  headerlist_payload_input("\xfb\\xc0\\x10\\x66\\x66\\x66\\x66\\x66\\x66\", "", In),
  Out = float(x8, 0xc010666666666666),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_infinity_x2_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xf9\\x7c\\x00\", "", In),
  BIn = [0xf9, 0x7c, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0x7c00),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_infinity_x2_encode).
test_rfc8949_float_infinity_x2_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xf9\\x7c\\x00\", "", In),
  BIn = [0xf9, 0x7c, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0x7c00),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_nan_x2_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xf9\\x7e\\x00\", "", In),
  BIn = [0xf9, 0x7e, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0x7e00),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_nan_x2_encode).
test_rfc8949_float_nan_x2_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xf9\\x7e\\x00\", "", In),
  BIn = [0xf9, 0x7e, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0x7e00),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_negative_infinity_x2_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xf9\\xfc\\x00\", "", In),
  BIn = [0xf9, 0xfc, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0xfc00),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_negative_infinity_x2_encode).
test_rfc8949_float_negative_infinity_x2_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xf9\\xfc\\x00\", "", In),
  BIn = [0xf9, 0xfc, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x2, 0xfc00),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_infinity_x4_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xfa\\x7f\\x80\\x00\\x00\", "", In),
  BIn = [0xfa, 0x7c, 0x80, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x4, 0x7c800000),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_infinity_x4_encode).
test_rfc8949_float_infinity_x4_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xfa\\x7f\\x80\\x00\\x00\", "", In),
  BIn = [0xfa, 0x7c, 0x80, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x4, 0x7c800000),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_nan_x4_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xfa\\x7e\\x00\\x00\\x00\", "", In),
  BIn = [0xfa, 0x7e, 0x00, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x4, 0x7e000000),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_nan_x4_encode).
test_rfc8949_float_nan_x4_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xfa\\x7e\\x00\\x00\\x00\", "", In),
  BIn = [0xfa, 0x7e, 0x00, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x4, 0x7e000000),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_negative_infinity_x4_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xfa\\xfc\\x80\\x00\\x00\", "", In),
  BIn = [0xfa, 0xfc, 0x80, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x4, 0xfc800000),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_negative_infinity_x4_encode).
test_rfc8949_float_negative_infinity_x4_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xfa\\xfc\\x80\\x00\\x00\", "", In),
  BIn = [0xfa, 0xfc, 0x80, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x4, 0xfc800000),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_infinity_x8_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xfb\\x7f\\xf0\\x00\\x00\\x00\\x00\\x00\\x00\", "", In),
  BIn = [0xfb, 0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x8, 0x7ff0000000000000),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_infinity_x8_encode).
test_rfc8949_float_infinity_x8_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xfb\\x7f\\xf0\\x00\\x00\\x00\\x00\\x00\\x00\", "", In),
  BIn = [0xfb, 0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x8, 0x7ff0000000000000),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_nan_x8_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xfb\\x7f\\xf8\\x00\\x00\\x00\\x00\\x00\\x00\", "", In),
  BIn = [0xfb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x8, 0x7ff8000000000000),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_nan_x8_encode).
test_rfc8949_float_nan_x8_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xfb\\x7f\\xf8\\x00\\x00\\x00\\x00\\x00\\x00\", "", In),
  BIn = [0xfb, 0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x8, 0x7ff8000000000000),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_float_negative_infinity_x8_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xfb\\xff\\xf0\\x00\\x00\\x00\\x00\\x00\\x00\", "", In),
  BIn = [0xfb, 0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x8, 0xfff0000000000000),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_float_negative_infinity_x8_encode).
test_rfc8949_float_negative_infinity_x8_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\0xfb\\xff\\xf0\\x00\\x00\\x00\\x00\\x00\\x00\", "", In),
  BIn = [0xfb, 0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = float(x8, 0xfff0000000000000),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_simple_false_decode :-
  headerlist_payload_input("\xf4\", "", In),
  Out = simple(i, 20),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_simple_false_encode).
test_rfc8949_simple_false_encode :-
  headerlist_payload_input("\xf4\", "", In),
  Out = simple(i, 20),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_simple_true_decode :-
  headerlist_payload_input("\xf5\", "", In),
  Out = simple(i, 21),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_simple_true_encode).
test_rfc8949_simple_true_encode :-
  headerlist_payload_input("\xf5\", "", In),
  Out = simple(i, 21),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_simple_null_decode :-
  headerlist_payload_input("\xf6\", "", In),
  Out = simple(i, 22),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_simple_null_encode).
test_rfc8949_simple_null_encode :-
  headerlist_payload_input("\xf6\", "", In),
  Out = simple(i, 22),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_simple_undefined_decode :-
  headerlist_payload_input("\xf7\", "", In),
  Out = simple(i, 23),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_simple_undefined_encode).
test_rfc8949_simple_undefined_encode :-
  headerlist_payload_input("\xf7\", "", In),
  Out = simple(i, 23),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_simple_16_decode :-
  headerlist_payload_input("\xf0\", "", In),
  Out = simple(i, 16),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_simple_16_encode).
test_rfc8949_simple_16_encode :-
  headerlist_payload_input("\xf0\", "", In),
  Out = simple(i, 16),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_simple_255_decode :-
  headerlist_payload_input("\xf8\\xff\", "", In),
  Out = simple(x1, 255),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_simple_255_encode).
test_rfc8949_simple_255_encode :-
  headerlist_payload_input("\xf8\\xff\", "", In),
  Out = simple(x1, 255),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_tag_0_2023d03d21T20c04c00Z_decode :-
  headerlist_payload_input("\xc0\\x74\\x32\\x30\\x31\\x33\\x2d\\x30\\x33\\x2d\\x32\\x31\\x54\\x32\\x30\\x3a\\x30\\x34\\x3a\\x30\\x30\\x5a\", "", In),
  Out = tag(i, 0, text(len(i, 20), "2013-03-21T20:04:00Z")),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_tag_0_2023d03d21T20c04c00Z_encode).
test_rfc8949_tag_0_2023d03d21T20c04c00Z_encode :-
  headerlist_payload_input("\xc0\\x74\\x32\\x30\\x31\\x33\\x2d\\x30\\x33\\x2d\\x32\\x31\\x54\\x32\\x30\\x3a\\x30\\x34\\x3a\\x30\\x30\\x5a\", "", In),
  Out = tag(i, 0, text(len(i, 20), "2013-03-21T20:04:00Z")),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_tag_1_1363896240_decode :-
  headerlist_payload_input("\xc1\\x1a\\x51\\x4b\\x67\\xb0\", "", In),
  Out = tag(i, 1, unsigned(x4, 1363896240)),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_tag_1_1363896240_encode).
test_rfc8949_tag_1_1363896240_encode :-
  headerlist_payload_input("\xc1\\x1a\\x51\\x4b\\x67\\xb0\", "", In),
  Out = tag(i, 1, unsigned(x4, 1363896240)),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_tag_1_1363896240d5_decode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xc1\\xfb\\x41\\xd4\\x52\\xd9\\xec\\x20\\x00\\x00\", "", In),
  BIn = [0xc1, 0xfb, 0x41, 0xd4, 0x52, 0xd9, 0xec, 0x20, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = tag(i, 1, float(x8, 0x41d452d9ec200000)),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_tag_1_1363896240d5_encode).
test_rfc8949_tag_1_1363896240d5_encode :-
  % Cannot use headerlist_payload_input/3, because there is a zero byte.
  % headerlist_payload_input("\xc1\\xfb\\x41\\xd4\\x52\\xd9\\xec\\x20\\x00\\x00\", "", In),
  BIn = [0xc1, 0xfb, 0x41, 0xd4, 0x52, 0xd9, 0xec, 0x20, 0x00, 0x00],
  maplist(code_char, BIn, In),
  Out = tag(i, 1, float(x8, 0x41d452d9ec200000)),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_tag_23_h01020304_decode :-
  headerlist_payload_input("\xd7\\x44\\x01\\x02\\x03\\x04\", "", In),
  Out = tag(i, 23, bytes(len(i, 4), [0x01, 0x02, 0x03, 0x04])),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_tag_23_h01020304_encode).
test_rfc8949_tag_23_h01020304_encode :-
  headerlist_payload_input("\xd7\\x44\\x01\\x02\\x03\\x04\", "", In),
  Out = tag(i, 23, bytes(len(i, 4), [0x01, 0x02, 0x03, 0x04])),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_tag_24_h6449455446_decode :-
  headerlist_payload_input("\xd8\\x18\\x45\\x64\\x49\\x45\\x54\\x46\", "", In),
  Out = tag(x1, 24, bytes(len(i, 5), [0x64, 0x49, 0x45, 0x54, 0x46])),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_tag_24_h6449455446_encode).
test_rfc8949_tag_24_h6449455446_encode :-
  headerlist_payload_input("\xd8\\x18\\x45\\x64\\x49\\x45\\x54\\x46\", "", In),
  Out = tag(x1, 24, bytes(len(i, 5), [0x64, 0x49, 0x45, 0x54, 0x46])),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_tag_32_httpcsswwwdexampledcom_decode :-
  headerlist_payload_input("\xd8\\x20\\x76\\x68\\x74\\x74\\x70\\x3a\\x2f\\x2f\\x77\\x77\\x77\\x2e\\x65\\x78\\x61\\x6d\\x70\\x6c\\x65\\x2e\\x63\\x6f\\x6d\", "", In),
  Out = tag(x1, 32, text(len(i, 22), "http://www.example.com")),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_tag_32_httpcsswwwdexampledcom_encode).
test_rfc8949_tag_32_httpcsswwwdexampledcom_encode :-
  headerlist_payload_input("\xd8\\x20\\x76\\x68\\x74\\x74\\x70\\x3a\\x2f\\x2f\\x77\\x77\\x77\\x2e\\x65\\x78\\x61\\x6d\\x70\\x6c\\x65\\x2e\\x63\\x6f\\x6d\", "", In),
  Out = tag(x1, 32, text(len(i, 22), "http://www.example.com")),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_bytes_h_decode :-
  headerlist_payload_input("\x40\", "", In),
  Out = bytes(len(i, 0), []),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_bytes_h_encode).
test_rfc8949_bytes_h_encode :-
  headerlist_payload_input("\x40\", "", In),
  Out = bytes(len(i, 0), []),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_bytes_h01020304_decode :-
  headerlist_payload_input("\x44\\x01\\x02\\x03\\x04\", "", In),
  Out = bytes(len(i, 4), [0x01, 0x02, 0x03, 0x04]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_bytes_h01020304_encode).
test_rfc8949_bytes_h01020304_encode :-
  headerlist_payload_input("\x44\\x01\\x02\\x03\\x04\", "", In),
  Out = bytes(len(i, 4), [0x01, 0x02, 0x03, 0x04]),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_text__decode :-
  headerlist_payload_input("\x60\", "", In),
  Out = text(len(i, 0), ""),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_text__encode).
test_rfc8949_text__encode :-
  headerlist_payload_input("\x60\", "", In),
  Out = text(len(i, 0), ""),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_text_a_decode :-
  headerlist_payload_input("\x61\\x61\", "", In),
  Out = text(len(i, 1), "a"),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_text_a_encode).
test_rfc8949_text_a_encode :-
  headerlist_payload_input("\x61\\x61\", "", In),
  Out = text(len(i, 1), "a"),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_text_IETF_decode :-
  headerlist_payload_input("\x64\\x49\\x45\\x54\\x46\", "", In),
  Out = text(len(i, 4), "IETF"),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_text_IETF_encode).
test_rfc8949_text_IETF_encode :-
  headerlist_payload_input("\x64\\x49\\x45\\x54\\x46\", "", In),
  Out = text(len(i, 4), "IETF"),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_text_quotebslash_decode :-
  headerlist_payload_input("\x62\\x22\\x5c\", "", In),
  Out = text(len(i, 2), "\"\\"),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_text_quotebslash_encode).
test_rfc8949_text_quotebslash_encode :-
  headerlist_payload_input("\x62\\x22\\x5c\", "", In),
  Out = text(len(i, 2), "\"\\"),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_text_u00fc_decode :-
  headerlist_payload_input("\x62\\xc3\\xbc\", "", In),
  Out = text(len(i, 2), "ü"),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_text_u00fc_encode).
test_rfc8949_text_u00fc_encode :-
  headerlist_payload_input("\x62\\xc3\\xbc\", "", In),
  Out = text(len(i, 2), "ü"),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_text_u6c34_decode :-
  headerlist_payload_input("\x63\\xe6\\xb0\\xb4\", "", In),
  Out = text(len(i, 3), "水"),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_text_u6c34_encode).
test_rfc8949_text_u6c34_encode :-
  headerlist_payload_input("\x63\\xe6\\xb0\\xb4\", "", In),
  Out = text(len(i, 3), "水"),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_text_ud800udd51_decode :-
  headerlist_payload_input("\x64\\xf0\\x90\\x85\\x91\", "", In),
  Out = text(len(i, 4), "𐅑"),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_text_ud800udd51_encode).
test_rfc8949_text_ud800udd51_encode :-
  headerlist_payload_input("\x64\\xf0\\x90\\x85\\x91\", "", In),
  Out = text(len(i, 4), "𐅑"),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_array__decode :-
  headerlist_payload_input("\x80\", "", In),
  Out = array(len(i, 0), []),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_array__encode).
test_rfc8949_array__encode :-
  headerlist_payload_input("\x80\", "", In),
  Out = array(len(i, 0), []),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_array_1_2_3_decode :-
  headerlist_payload_input("\x83\\x01\\x02\\x03\", "", In),
  Out = array(len(i, 3), [unsigned(i, 1), unsigned(i, 2), unsigned(i, 3)]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_array_1_2_3_encode).
test_rfc8949_array_1_2_3_encode :-
  headerlist_payload_input("\x83\\x01\\x02\\x03\", "", In),
  Out = array(len(i, 3), [unsigned(i, 1), unsigned(i, 2), unsigned(i, 3)]),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_array_1_23_45_decode :-
  headerlist_payload_input("\x83\\x01\\x82\\x02\\x03\\x82\\x04\\x05\", "", In),
  Out = array(len(i, 3), [
    unsigned(i, 1),
    array(len(i, 2), [unsigned(i, 2), unsigned(i, 3)]),
    array(len(i, 2), [unsigned(i, 4), unsigned(i, 5)])
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_array_1_23_45_encode).
test_rfc8949_array_1_23_45_encode :-
  headerlist_payload_input("\x83\\x01\\x82\\x02\\x03\\x82\\x04\\x05\", "", In),
  Out = array(len(i, 3), [
    unsigned(i, 1),
    array(len(i, 2), [unsigned(i, 2), unsigned(i, 3)]),
    array(len(i, 2), [unsigned(i, 4), unsigned(i, 5)])
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_array_1ddd25_decode :-
  headerlist_payload_input("\x98\\x19\\x01\\x02\\x03\\x04\\x05\\x06\\x07\\x08\\x09\\x0a\\x0b\\x0c\\x0d\\x0e\\x0f\\x10\\x11\\x12\\x13\\x14\\x15\\x16\\x17\\x18\\x18\\x18\\x19\", "", In),
  Out = array(len(x1, 25), [
    unsigned(i, 1), unsigned(i, 2), unsigned(i, 3), unsigned(i, 4), unsigned(i, 5),
    unsigned(i, 6), unsigned(i, 7), unsigned(i, 8), unsigned(i, 9), unsigned(i, 10),
    unsigned(i, 11), unsigned(i, 12), unsigned(i, 13), unsigned(i, 14), unsigned(i, 15),
    unsigned(i, 16), unsigned(i, 17), unsigned(i, 18), unsigned(i, 19), unsigned(i, 20),
    unsigned(i, 21), unsigned(i, 22), unsigned(i, 23), unsigned(x1, 24), unsigned(x1, 25)
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_array_1ddd25_encode).
test_rfc8949_array_1ddd25_encode :-
  headerlist_payload_input("\x98\\x19\\x01\\x02\\x03\\x04\\x05\\x06\\x07\\x08\\x09\\x0a\\x0b\\x0c\\x0d\\x0e\\x0f\\x10\\x11\\x12\\x13\\x14\\x15\\x16\\x17\\x18\\x18\\x18\\x19\", "", In),
  Out = array(len(x1, 25), [
    unsigned(i, 1), unsigned(i, 2), unsigned(i, 3), unsigned(i, 4), unsigned(i, 5),
    unsigned(i, 6), unsigned(i, 7), unsigned(i, 8), unsigned(i, 9), unsigned(i, 10),
    unsigned(i, 11), unsigned(i, 12), unsigned(i, 13), unsigned(i, 14), unsigned(i, 15),
    unsigned(i, 16), unsigned(i, 17), unsigned(i, 18), unsigned(i, 19), unsigned(i, 20),
    unsigned(i, 21), unsigned(i, 22), unsigned(i, 23), unsigned(x1, 24), unsigned(x1, 25)
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_map__decode :-
  headerlist_payload_input("\xa0\", "", In),
  Out = map(len(i, 0), []),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_map__encode).
test_rfc8949_map__encode :-
  headerlist_payload_input("\xa0\", "", In),
  Out = map(len(i, 0), []),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_map_12_34_decode :-
  headerlist_payload_input("\xa2\\x01\\x02\\x03\\x04\", "", In),
  Out = map(len(i, 2), [
    unsigned(i, 1)-unsigned(i, 2),
    unsigned(i, 3)-unsigned(i, 4)
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_map_12_34_encode).
test_rfc8949_map_12_34_encode :-
  headerlist_payload_input("\xa2\\x01\\x02\\x03\\x04\", "", In),
  Out = map(len(i, 2), [
    unsigned(i, 1)-unsigned(i, 2),
    unsigned(i, 3)-unsigned(i, 4)
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_map_a1_b23_decode :-
  headerlist_payload_input("\xa2\\x61\\x61\\x01\\x61\\x62\\x82\\x02\\x03\", "", In),
  Out = map(len(i, 2), [
    text(len(i, 1), "a")-unsigned(i, 1),
    text(len(i, 1), "b")-array(len(i, 2), [unsigned(i, 2), unsigned(i, 3)])
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_map_a1_b23_encode).
test_rfc8949_map_a1_b23_encode :-
  headerlist_payload_input("\xa2\\x61\\x61\\x01\\x61\\x62\\x82\\x02\\x03\", "", In),
  Out = map(len(i, 2), [
    text(len(i, 1), "a")-unsigned(i, 1),
    text(len(i, 1), "b")-array(len(i, 2), [unsigned(i, 2), unsigned(i, 3)])
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_array_a_bc_decode :-
  headerlist_payload_input("\x82\\x61\\x61\\xa1\\x61\\x62\\x61\\x63\", "", In),
  Out = array(len(i, 2), [
    text(len(i, 1), "a"),
    map(len(i, 1), [text(len(i, 1), "b")-text(len(i, 1), "c")])
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_array_a_bc_encode).
test_rfc8949_array_a_bc_encode :-
  headerlist_payload_input("\x82\\x61\\x61\\xa1\\x61\\x62\\x61\\x63\", "", In),
  Out = array(len(i, 2), [
    text(len(i, 1), "a"),
    map(len(i, 1), [text(len(i, 1), "b")-text(len(i, 1), "c")])
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

test_rfc8949_map_aA_bB_cC_dD_eE_decode :-
  headerlist_payload_input("\xa5\\x61\\x61\\x61\\x41\\x61\\x62\\x61\\x42\\x61\\x63\\x61\\x43\\x61\\x64\\x61\\x44\\x61\\x65\\x61\\x45\", "", In),
  Out = map(len(i, 5), [
    text(len(i, 1), "a")-text(len(i, 1), "A"),
    text(len(i, 1), "b")-text(len(i, 1), "B"),
    text(len(i, 1), "c")-text(len(i, 1), "C"),
    text(len(i, 1), "d")-text(len(i, 1), "D"),
    text(len(i, 1), "e")-text(len(i, 1), "E")
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_map_aA_bB_cC_dD_eE_encode).
test_rfc8949_map_aA_bB_cC_dD_eE_encode :-
  headerlist_payload_input("\xa5\\x61\\x61\\x61\\x41\\x61\\x62\\x61\\x42\\x61\\x63\\x61\\x43\\x61\\x64\\x61\\x44\\x61\\x65\\x61\\x45\", "", In),
  Out = map(len(i, 5), [
    text(len(i, 1), "a")-text(len(i, 1), "A"),
    text(len(i, 1), "b")-text(len(i, 1), "B"),
    text(len(i, 1), "c")-text(len(i, 1), "C"),
    text(len(i, 1), "d")-text(len(i, 1), "D"),
    text(len(i, 1), "e")-text(len(i, 1), "E")
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

nwdet(test_rfc8949_bytes_indefinite_h0102_h030405_decode).
test_rfc8949_bytes_indefinite_h0102_h030405_decode :-
  headerlist_payload_input("\x5f\\x42\\x01\\x02\\x43\\x03\\x04\\x05\\xff\", "", In),
  Out = bytes(*, [
    bytes(len(i, 2), [0x01, 0x02]),
    bytes(len(i, 3), [0x03, 0x04, 0x05])
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_bytes_indefinite_h0102_h030405_encode).
test_rfc8949_bytes_indefinite_h0102_h030405_encode :-
  headerlist_payload_input("\x5f\\x42\\x01\\x02\\x43\\x03\\x04\\x05\\xff\", "", In),
  Out = bytes(*, [
    bytes(len(i, 2), [0x01, 0x02]),
    bytes(len(i, 3), [0x03, 0x04, 0x05])
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

nwdet(test_rfc8949_text_indefinite_strea_ming_decode).
test_rfc8949_text_indefinite_strea_ming_decode :-
  headerlist_payload_input("\x7f\\x65\\x73\\x74\\x72\\x65\\x61\\x64\\x6d\\x69\\x6e\\x67\\xff\", "", In),
  Out = text(*, [
    text(len(i, 5), "strea"),
    text(len(i, 4), "ming")
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_text_indefinite_strea_ming_encode).
test_rfc8949_text_indefinite_strea_ming_encode :-
  headerlist_payload_input("\x7f\\x65\\x73\\x74\\x72\\x65\\x61\\x64\\x6d\\x69\\x6e\\x67\\xff\", "", In),
  Out = text(*, [
    text(len(i, 5), "strea"),
    text(len(i, 4), "ming")
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

nwdet(test_rfc8949_array_indefinite__decode).
test_rfc8949_array_indefinite__decode :-
  headerlist_payload_input("\x9f\\xff\", "", In),
  Out = array(*, []),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_array_indefinite__encode).
test_rfc8949_array_indefinite__encode :-
  headerlist_payload_input("\x9f\\xff\", "", In),
  Out = array(*, []),
  meta_test_rfc8949_encode(In, Out),
true.

nwdet(test_rfc8949_array_indefinite_1_23_i45_decode).
test_rfc8949_array_indefinite_1_23_i45_decode :-
  headerlist_payload_input("\x9f\\x01\\x82\\x02\\x03\\x9f\\x04\\x05\\xff\\xff\", "", In),
  Out = array(*, [
    unsigned(i, 1),
    array(len(i, 2), [unsigned(i, 2), unsigned(i, 3)]),
    array(*, [unsigned(i, 4), unsigned(i, 5)])
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_array_indefinite_1_23_i45_encode).
test_rfc8949_array_indefinite_1_23_i45_encode :-
  headerlist_payload_input("\x9f\\x01\\x82\\x02\\x03\\x9f\\x04\\x05\\xff\\xff\", "", In),
  Out = array(*, [
    unsigned(i, 1),
    array(len(i, 2), [unsigned(i, 2), unsigned(i, 3)]),
    array(*, [unsigned(i, 4), unsigned(i, 5)])
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

nwdet(test_rfc8949_array_indefinite_1_23_45_decode).
test_rfc8949_array_indefinite_1_23_45_decode :-
  headerlist_payload_input("\x9f\\x01\\x82\\x02\\x03\\x82\\x04\\x05\\xff\", "", In),
  Out = array(*, [
    unsigned(i, 1),
    array(len(i, 2), [unsigned(i, 2), unsigned(i, 3)]),
    array(len(i, 2), [unsigned(i, 4), unsigned(i, 5)])
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_array_indefinite_1_23_45_encode).
test_rfc8949_array_indefinite_1_23_45_encode :-
  headerlist_payload_input("\x9f\\x01\\x82\\x02\\x03\\x82\\x04\\x05\\xff\", "", In),
  Out = array(*, [
    unsigned(i, 1),
    array(len(i, 2), [unsigned(i, 2), unsigned(i, 3)]),
    array(len(i, 2), [unsigned(i, 4), unsigned(i, 5)])
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

nwdet(test_rfc8949_array_1_23_i45_decode).
test_rfc8949_array_1_23_i45_decode :-
  headerlist_payload_input("\x83\\x01\\x82\\x02\\x03\\x9f\\x04\\x05\\xff\", "", In),
  Out = array(len(i, 3), [
    unsigned(i, 1),
    array(len(i, 2), [unsigned(i, 2), unsigned(i, 3)]),
    array(*, [unsigned(i, 4), unsigned(i, 5)])
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_array_1_23_i45_encode).
test_rfc8949_array_1_23_i45_encode :-
  headerlist_payload_input("\x83\\x01\\x82\\x02\\x03\\x9f\\x04\\x05\\xff\", "", In),
  Out = array(len(i, 3), [
    unsigned(i, 1),
    array(len(i, 2), [unsigned(i, 2), unsigned(i, 3)]),
    array(*, [unsigned(i, 4), unsigned(i, 5)])
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

nwdet(test_rfc8949_array_1_i23_45_decode).
test_rfc8949_array_1_i23_45_decode :-
  headerlist_payload_input("\x83\\x01\\x9f\\x02\\x03\\xff\\x82\\x04\\x05\", "", In),
  Out = array(len(i, 3), [
    unsigned(i, 1),
    array(*, [unsigned(i, 2), unsigned(i, 3)]),
    array(len(i, 2), [unsigned(i, 4), unsigned(i, 5)])
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_array_1_i23_45_encode).
test_rfc8949_array_1_i23_45_encode :-
  headerlist_payload_input("\x83\\x01\\x9f\\x02\\x03\\xff\\x82\\x04\\x05\", "", In),
  Out = array(len(i, 3), [
    unsigned(i, 1),
    array(*, [unsigned(i, 2), unsigned(i, 3)]),
    array(len(i, 2), [unsigned(i, 4), unsigned(i, 5)])
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

nwdet(test_rfc8949_array_indefinite_1ddd25_decode).
test_rfc8949_array_indefinite_1ddd25_decode :-
  headerlist_payload_input("\x9f\\x01\\x02\\x03\\x04\\x05\\x06\\x07\\x08\\x09\\x0a\\x0b\\x0c\\x0d\\x0e\\x0f\\x10\\x11\\x12\\x13\\x14\\x15\\x16\\x17\\x18\\x18\\x18\\x19\\xff\", "", In),
  Out = array(*, [
    unsigned(i, 1), unsigned(i, 2), unsigned(i, 3), unsigned(i, 4), unsigned(i, 5),
    unsigned(i, 6), unsigned(i, 7), unsigned(i, 8), unsigned(i, 9), unsigned(i, 10),
    unsigned(i, 11), unsigned(i, 12), unsigned(i, 13), unsigned(i, 14), unsigned(i, 15),
    unsigned(i, 16), unsigned(i, 17), unsigned(i, 18), unsigned(i, 19), unsigned(i, 20),
    unsigned(i, 21), unsigned(i, 22), unsigned(i, 23), unsigned(x1, 24), unsigned(x1, 25)
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_array_indefinite_1ddd25_encode).
test_rfc8949_array_indefinite_1ddd25_encode :-
  headerlist_payload_input("\x9f\\x01\\x02\\x03\\x04\\x05\\x06\\x07\\x08\\x09\\x0a\\x0b\\x0c\\x0d\\x0e\\x0f\\x10\\x11\\x12\\x13\\x14\\x15\\x16\\x17\\x18\\x18\\x18\\x19\\xff\", "", In),
  Out = array(*, [
    unsigned(i, 1), unsigned(i, 2), unsigned(i, 3), unsigned(i, 4), unsigned(i, 5),
    unsigned(i, 6), unsigned(i, 7), unsigned(i, 8), unsigned(i, 9), unsigned(i, 10),
    unsigned(i, 11), unsigned(i, 12), unsigned(i, 13), unsigned(i, 14), unsigned(i, 15),
    unsigned(i, 16), unsigned(i, 17), unsigned(i, 18), unsigned(i, 19), unsigned(i, 20),
    unsigned(i, 21), unsigned(i, 22), unsigned(i, 23), unsigned(x1, 24), unsigned(x1, 25)
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

nwdet(test_rfc8949_map_indefinite_a1_bi23_decode).
test_rfc8949_map_indefinite_a1_bi23_decode :-
  headerlist_payload_input("\xbf\\x61\\x61\\x01\\x61\\x62\\x9f\\x02\\x03\\xff\\xff\", "", In),
  Out = map(*, [
    text(len(i, 1), "a")-unsigned(i, 1),
    text(len(i, 1), "b")-array(*, [
      unsigned(i, 2), unsigned(i, 3)
    ])
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_map_indefinite_a1_bi23_encode).
test_rfc8949_map_indefinite_a1_bi23_encode :-
  headerlist_payload_input("\xbf\\x61\\x61\\x01\\x61\\x62\\x9f\\x02\\x03\\xff\\xff\", "", In),
  Out = map(*, [
    text(len(i, 1), "a")-unsigned(i, 1),
    text(len(i, 1), "b")-array(*, [
      unsigned(i, 2), unsigned(i, 3)
    ])
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

nwdet(test_rfc8949_array_a_ibc_decode).
test_rfc8949_array_a_ibc_decode :-
  headerlist_payload_input("\x82\\x61\\x61\\xbf\\x61\\x62\\x61\\x63\\xff\", "", In),
  Out = array(len(i, 2), [
    text(len(i, 1), "a"),
    map(*, [text(len(i, 1), "b")-text(len(i, 1), "c")])
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_array_a_ibc_encode).
test_rfc8949_array_a_ibc_encode :-
  headerlist_payload_input("\x82\\x61\\x61\\xbf\\x61\\x62\\x61\\x63\\xff\", "", In),
  Out = array(len(i, 2), [
    text(len(i, 1), "a"),
    map(*, [text(len(i, 1), "b")-text(len(i, 1), "c")])
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

nwdet(test_rfc8949_map_indefinite_funtrue_amtm2_decode).
test_rfc8949_map_indefinite_funtrue_amtm2_decode :-
  headerlist_payload_input("\xbf\\x63\\x46\\x75\\x6e\\xf5\\x63\\x41\\x6d\\x74\\x21\\xff\", "", In),
  Out = map(*, [
    text(len(i, 3), "Fun")-simple(i, 21),
    text(len(i, 3), "Amt")-negative(i, -2)
  ]),
  meta_test_rfc8949_decode(In, Out),
true.

nwdet(test_rfc8949_map_indefinite_funtrue_amtm2_encode).
test_rfc8949_map_indefinite_funtrue_amtm2_encode :-
  headerlist_payload_input("\xbf\\x63\\x46\\x75\\x6e\\xf5\\x63\\x41\\x6d\\x74\\x21\\xff\", "", In),
  Out = map(*, [
    text(len(i, 3), "Fun")-simple(i, 21),
    text(len(i, 3), "Amt")-negative(i, -2)
  ]),
  meta_test_rfc8949_encode(In, Out),
true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RFC 8949 (Appendix A) End   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prefix_module_predicates(Prefix, M, Ps) :-
  ( findall(T,
      ( T = M:P,
        current_predicate(M:P/_),
        atom_chars(P, Name),
        phrase(seq(Prefix), Name, _)
      ),
      Ps
    )
  ; Ps = []
  ).

run_log_and_halt(Ts) :-
  foldl(run_test, Ts, t(0, 0, 0, 0, 0), t(Np, Nf, Nd, Nds, Nt)),
  ( (Nf > 0 ; Nd > 0) -> nl; true ),
  ( Nf > 0 ->
    write(Nf), writen(' tests failed!')
  ; true
  ),
  ( Nd > 0 ->
    write(Nd), writen(' tests were not well-behaved deterministic!')
  ; true
  ),
  ( Np > 0 ->
    write(Np), writen(' tests passed.')
  ; writen('No tests to run.')
  ),
  ( Nds > 0 ->
    write(Nds), writen(' tests not well-behaved deterministic supreessed.')
  ; true
  ),
  ( Nt == Np -> ExitCode = 0 ; ExitCode = 1 ),
  halt(ExitCode),
true.

run_tests :- run_tests("test_").

run_tests(Prefix) :- run_tests(Prefix, cbor_tests).

run_tests(Prefix, Module) :-
  prefix_module_predicates(Prefix, Module, Ts),
  run_log_and_halt(Ts),
true.

:- meta_predicate(run_test(0, ?, ?)).

run_test(Test, t(Np0, Nf0, Nd0, Nds0, Nt0), t(Np, Nf, Nd, Nds, Nt)) :-
  Nt is Nt0 + 1,
  catch(
    ( call_cleanup(Test, Result = pass) ; Result = failed ),
    Err,
    ( Result = failed,
      write(Test), write(' throws exception '), write(Err), nl
    )
  ),
  ( Result == pass ->
    Np is Np0 + 1, Nf is Nf0, Nd is Nd0, Nds is Nds0,
    ( nwdet_ok(Test) ->
      write(Test), write(' IS WELL-BEHAVED DETERMINISTIC!'), nl
    ; true
    )
  ; Result == failed ->
    Np is Np0, Nf is Nf0 + 1, Nd is Nd0, Nds is Nds0,
    write(Test), write(' failed!'), nl
  ; var(Result) ->
    ( nwdet_ok(Test) ->
      Np is Np0 + 1, Nf is Nf0, Nd is Nd0, Nds is Nds0 + 1
    ; Np is Np0, Nf is Nf0, Nd is Nd0 + 1, Nds is Nds0,
      write(Test), write(' is not well-behaved deterministic!'), nl
    )
  ),
  !.

writen(X) :- write(X), nl.
