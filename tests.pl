:- module(cbor_tests, [
  run_tests/0
]).

:- use_module(cbor, [
  cbor_item//1
]).

:- use_module(library(dcgs), [seq//1, phrase/2, phrase/3]).
:- use_module(library(lists), [
  append/3, foldl/4, maplist/3, length/2
]).
:- use_module(library(iso_ext), [call_cleanup/2]).

clpz:monotonic.

pair_unpair_(K-V, [K, V | L], L).

headerlist_payload_input(H, Payload, In) :-
  maplist(char_code, H, Header),
  append(Header, Payload, In).

nwdet_ok(_:T) :- nwdet(T).
:- discontiguous(nwdet/1).

test_item_decode_10 :-
  headerlist_payload_input("\x0a\", "", In),
  phrase(cbor_item(X), In),
  X == unsigned(i, 10),
true.

test_item_encode_10 :-
  Item = unsigned(_, 10),
  findall(Out, cbor_item(Item, Out, []), Answers),
  Answers == [
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
true.

test_item_encode_500 :-
  Item = unsigned(_, 500),
  findall(Out, cbor_item(Item, Out, []), Answers),
  Answers == [
    [25, 1, 244],
    [26, 0, 0, 1, 244],
    [27, 0, 0, 0, 0, 0, 0, 1, 244]
  ],
true.

test_item_decode_negative_10 :-
  headerlist_payload_input("\x29\", "", In),
  phrase(cbor_item(X), In),
  X == negative(i, -10),
true.

test_item_encode_negative_10 :-
  Item = negative(_, -10),
  findall(Out, cbor_item(Item, Out, []), Answers),
  Answers == [
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
true.

test_item_encode_negative_500 :-
  Item = negative(_, -500),
  findall(Out, cbor_item(Item, Out, []), Answers),
  Answers == [
    [57, 1, 243],
    [58, 0, 0, 1, 243],
    [59, 0, 0, 0, 0, 0, 0, 1, 243]
  ],
true.

test_item_decode_5_bytes :-
  Len = 5,
  length(Payload, Len),
  headerlist_payload_input("\x45\", Payload, In),
  phrase(cbor_item(X), In),
  X == bytes(len(i, Len), Payload),
true.

test_item_decode_500_bytes :-
  Len = 500,
  length(Payload, Len),
  headerlist_payload_input("\x59\\x01\\xf4\", Payload, In),
  phrase(cbor_item(X), In),
  X == bytes(len(x2, Len), Payload),
true.

test_item_decode_bytes_indefinite :-
  headerlist_payload_input("\x5f\\x44\\xaa\\xbb\\xcc\\xdd\\x43\\xee\\xff\\x99\\xff\", "", In),
  phrase(cbor_item(X), In),
  X == bytes(*, [
    bytes(len(i, 4), [0xaa, 0xbb, 0xcc, 0xdd]),
    bytes(len(i, 3), [0xee, 0xff, 0x99])
  ]),
true.

test_item_decode_text_ascii_small :-
  Text = "ascii rules!",
  Len = 12,
  length(Text, Len),
  maplist(char_code, Text, Payload),
  headerlist_payload_input("\x6c\", Payload, In),
  phrase(cbor_item(X), In),
  X == text(len(i, Len), Text),
true.

test_item_decode_text_ascii_medium :-
  Text = "ascii text with !@#$%*()_-+=\", 0123456789 and LF\n",
  Len = 49,
  length(Text, Len),
  maplist(char_code, Text, Payload),
  headerlist_payload_input("\x78\\x31\", Payload, In),
  phrase(cbor_item(X), In),
  X == text(len(x1, Len), Text),
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
true.

test_item_decode_text_indefinite :-
  headerlist_payload_input("\x7f\\x63\\x61\\x62\\x63\\x62\\x30\\x31\\xff\", "", In),
  phrase(cbor_item(X), In),
  X == text(*, [
    text(len(i, 3), "abc"),
    text(len(i, 2), "01")
  ]),
true.

test_item_decode_array :-
  Items = [unsigned(i, 10), unsigned(x2, 500), negative(i, -10), negative(x2, -500)],
  Len = 4,
  length(Items, Len),
  once(foldl(cbor_item, Items, Payload, [])),
  headerlist_payload_input("\x84\", Payload, In),
  phrase(cbor_item(X), In),
  X == array(len(i, Len), Items),
true.

test_item_decode_array_1_23_45 :-
  headerlist_payload_input("\x83\\x01\\x82\\x02\\x03\\x82\\x04\\x05\", "", In),
  phrase(cbor_item(X), In),
  X == array(len(i, 3), [
    unsigned(i, 1),
    array(len(i, 2), [unsigned(i, 2), unsigned(i, 3)]),
    array(len(i, 2), [unsigned(i, 4), unsigned(i, 5)])
  ]),
true.

test_item_decode_indefinite_array_i1_23_45 :-
  headerlist_payload_input("\x9f\\x01\\x82\\x02\\x03\\x82\\x04\\x05\\xff\", "", In),
  phrase(cbor_item(X), In),
  X == array(*, [
    unsigned(i, 1),
    array(len(i, 2), [unsigned(i, 2), unsigned(i, 3)]),
    array(len(i, 2), [unsigned(i, 4), unsigned(i, 5)])
  ]),
true.

test_item_decode_indefinite_array_i1_23_i45 :-
  headerlist_payload_input("\x9f\\x01\\x82\\x02\\x03\\x9f\\x04\\x05\\xff\\xff\", "", In),
  phrase(cbor_item(X), In),
  X == array(*, [
    unsigned(i, 1),
    array(len(i, 2), [unsigned(i, 2), unsigned(i, 3)]),
    array(*, [unsigned(i, 4), unsigned(i, 5)])
  ]),
true.

test_item_decode_array_1_23_i45 :-
  headerlist_payload_input("\x83\\x01\\x82\\x02\\x03\\x9f\\x04\\x05\\xff\", "", In),
  phrase(cbor_item(X), In),
  X == array(len(i, 3), [
    unsigned(i, 1),
    array(len(i, 2), [unsigned(i, 2), unsigned(i, 3)]),
    array(*, [unsigned(i, 4), unsigned(i, 5)])
  ]),
true.

test_item_decode_map :-
  Pairs = [unsigned(i, 10)-unsigned(x2, 500), negative(i, -10)-negative(x2, -500)],
  foldl(pair_unpair_, Pairs, Items, []),
  Len = 2,
  length(Pairs, Len),
  once(foldl(cbor_item, Items, Payload, [])),
  headerlist_payload_input("\xa2\", Payload, In),
  phrase(cbor_item(X), In),
  X == map(len(i, Len), Pairs),
true.

test_item_decode_indefinite_map_text :-
  headerlist_payload_input("\xbf\\x63\\x46\\x75\\x6e\\xf5\\x63\\x41\\x6d\\x74\\x21\\xff\", "", In),
  phrase(cbor_item(X), In),
  X == map(*, [
    text(len(i, 3), "Fun")-simple(i, 21),
    text(len(i, 3), "Amt")-negative(i, -2)
  ]),
true.

test_item_decode_indefinite_map_unsigned :-
  headerlist_payload_input("\xbf\\x20\\xf5\\x01\\x21\\xff\", "", In),
  phrase(cbor_item(X), In),
  X == map(*, [
    negative(i, -1)-simple(i, 21),
    unsigned(i, 1)-negative(i, -2)
  ]),
true.

test_item_decode_tag_small :-
  TagNumber = 10,
  Item = unsigned(x2, 500),
  once(phrase(cbor_item(Item), Payload)),
  headerlist_payload_input("\xca\", Payload, In),
  phrase(cbor_item(X), In),
  X == tag(tag(i, TagNumber), Item),
true.

test_item_decode_tag_big :-
  TagNumber = 500,
  Item = unsigned(x2, 500),
  once(phrase(cbor_item(Item), Payload)),
  headerlist_payload_input("\xd9\\x01\\xf4\", Payload, In),
  phrase(cbor_item(X), In),
  X == tag(tag(x2, TagNumber), Item),
true.

test_item_decode_simple_small :-
  headerlist_payload_input("\xf4\", "", In),
  phrase(cbor_item(X), In),
  X == simple(i, 20),
true.

test_item_decode_simple_big :-
  headerlist_payload_input("\xf8\\x78\", "", In),
  phrase(cbor_item(X), In),
  X == simple(x1, 120),
true.

% TODO: add tests for floats

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
