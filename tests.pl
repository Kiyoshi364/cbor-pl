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

pair_unpair_(K-V, [K, V | L], L).

headerlist_payload_input(H, Payload, In) :-
  maplist(char_code, H, Header),
  append(Header, Payload, In).

nwdet_ok(_:T) :- nwdet(T).
:- discontiguous(nwdet/1).

test_item_encode_10 :-
  maplist(char_code, "\x0a\", In),
  phrase(cbor_item(X), In),
  X == unsigned(10),
true.

test_item_decode_10 :-
  Item = unsigned(10),
  findall(Out, cbor_item(Item, Out, []), Answers),
  Answers == [
    [10],
    [24, 10],
    [25, 0, 10],
    [26, 0, 0, 0, 10],
    [27, 0, 0, 0, 0, 0, 0, 0, 10]
  ],
true.

test_item_encode_500 :-
  maplist(char_code, "\x19\\x01\\xf4\", In),
  phrase(cbor_item(X), In),
  X == unsigned(500),
true.

test_item_decode_500 :-
  Item = unsigned(500),
  findall(Out, cbor_item(Item, Out, []), Answers),
  Answers == [
    [25, 1, 244],
    [26, 0, 0, 1, 244],
    [27, 0, 0, 0, 0, 0, 0, 1, 244]
  ],
true.

test_item_encode_negative_10 :-
  maplist(char_code, "\x29\", In),
  phrase(cbor_item(X), In),
  X == negative(-10),
true.

test_item_decode_negative_10 :-
  Item = negative(-10),
  findall(Out, cbor_item(Item, Out, []), Answers),
  Answers == [
    [41],
    [56, 9],
    [57, 0, 9],
    [58, 0, 0, 0, 9],
    [59, 0, 0, 0, 0, 0, 0, 0, 9]
  ],
true.

test_item_encode_negative_500 :-
  maplist(char_code, "\x39\\x01\\xf3\", In),
  phrase(cbor_item(X), In),
  X == negative(-500),
true.

test_item_decode_negative_500 :-
  Item = negative(-500),
  findall(Out, cbor_item(Item, Out, []), Answers),
  Answers == [
    [57, 1, 243],
    [58, 0, 0, 1, 243],
    [59, 0, 0, 0, 0, 0, 0, 1, 243]
  ],
true.

test_item_encode_5_bytes :-
  Len = 5,
  length(Payload, Len),
  headerlist_payload_input("\x45\", Payload, In),
  phrase(cbor_item(X), In),
  X == bytes(len(Len), Payload),
true.

test_item_encode_500_bytes :-
  Len = 500,
  length(Payload, Len),
  headerlist_payload_input("\x59\\x01\\xf4\", Payload, In),
  phrase(cbor_item(X), In),
  X == bytes(len(Len), Payload),
true.

nwdet(test_item_encode_small_ascii_text).
test_item_encode_small_ascii_text :-
  Text = "ascii rules!",
  Len = 12,
  length(Text, Len),
  maplist(char_code, Text, Payload),
  headerlist_payload_input("\x6c\", Payload, In),
  phrase(cbor_item(X), In),
  X == text(len(Len), Text),
true.

nwdet(test_item_encode_medium_ascii_text).
test_item_encode_medium_ascii_text :-
  Text = "ascii text with !@#$%*()_-+=\", 0123456789 and LF\n",
  Len = 49,
  length(Text, Len),
  maplist(char_code, Text, Payload),
  headerlist_payload_input("\x78\\x31\", Payload, In),
  phrase(cbor_item(X), In),
  X == text(len(Len), Text),
true.

% TODO: add tests for utf8 text

test_item_encode_array :-
  Items = [unsigned(10), unsigned(500), negative(-10), negative(-500)],
  Len = 4,
  length(Items, Len),
  once(foldl(cbor_item, Items, Payload, [])),
  headerlist_payload_input("\x84\", Payload, In),
  phrase(cbor_item(X), In),
  X == array(len(Len), Items),
true.

test_item_encode_map :-
  Pairs = [unsigned(10)-unsigned(500), negative(-10)-negative(-500)],
  foldl(pair_unpair_, Pairs, Items, []),
  Len = 2,
  length(Pairs, Len),
  once(foldl(cbor_item, Items, Payload, [])),
  headerlist_payload_input("\xa2\", Payload, In),
  phrase(cbor_item(X), In),
  X == map(len(Len), Pairs),
true.

test_item_encode_tag_small :-
  TagNumber = 10,
  Item = unsigned(500),
  once(phrase(cbor_item(Item), Payload)),
  headerlist_payload_input("\xca\", Payload, In),
  phrase(cbor_item(X), In),
  X == tag(tag(TagNumber), Item),
true.

test_item_encode_tag_big :-
  TagNumber = 500,
  Item = unsigned(500),
  once(phrase(cbor_item(Item), Payload)),
  headerlist_payload_input("\xd9\\x01\\xf4\", Payload, In),
  phrase(cbor_item(X), In),
  X == tag(tag(TagNumber), Item),
true.

run_tests :-
  ( findall(T,
      ( T = M:P,
        M = cbor_tests,
        current_predicate(M:P/_),
        atom_chars(P, Name),
        phrase(seq("test_"), Name, _)
      ),
      Ts
    )
  ; Ts = []
  ),
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
