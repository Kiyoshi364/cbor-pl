:- module(cbor, [
  cbor_item//1, cbor//1
]).

:- use_module(library(dcgs), [seq//1]).
:- use_module(library(clpz), [
  (#<)/2, (#=)/2, (in)/2,
  op(700, xfx, #<),
  op(700, xfx, #=),
  op(700, xfx, in),
  op(450, xfx, ..),
  op(150, fx, #)
]).
:- use_module(library(charsio), [chars_utf8bytes/2]).
:- use_module(library(lists), [foldl/4, length/2]).

clpz:monotonic.

cbor(X) --> cbor_item(X).

cbor_pair(K-V) --> cbor_item(K), cbor_item(V).

byte( X) :- X in 0x00..0xff.
short(X) :- X in 0x00..0xffff.
word( X) :- X in 0x00..0xffffffff.
quad( X) :- X in 0x00..0xffffffffffffffff.

byte(X) --> { byte(X) }, [X].

header_major_minor(Header, Major, Minor) :-
  Major in 0..7,
  Minor in 0x00..0x1f,
  #Header #= (#Major << 5) \/ #Minor,
  #Major #= #Header >> 5,
  #Minor #= #Header /\ 0x1f,
true.

cbor_major_minor(Major, Minor) -->
  byte(H),
  { header_major_minor(H, Major, Minor) }.

cbor_major_value(Major, Value) -->
  cbor_major_minor(Major, Minor),
  cbor_minor_value(Minor, Value).

cbor_minor_value( 0, val(i,  0)) --> [].
cbor_minor_value( 1, val(i,  1)) --> [].
cbor_minor_value( 2, val(i,  2)) --> [].
cbor_minor_value( 3, val(i,  3)) --> [].
cbor_minor_value( 4, val(i,  4)) --> [].
cbor_minor_value( 5, val(i,  5)) --> [].
cbor_minor_value( 6, val(i,  6)) --> [].
cbor_minor_value( 7, val(i,  7)) --> [].
cbor_minor_value( 8, val(i,  8)) --> [].
cbor_minor_value( 9, val(i,  9)) --> [].
cbor_minor_value(10, val(i, 10)) --> [].
cbor_minor_value(11, val(i, 11)) --> [].
cbor_minor_value(12, val(i, 12)) --> [].
cbor_minor_value(13, val(i, 13)) --> [].
cbor_minor_value(14, val(i, 14)) --> [].
cbor_minor_value(15, val(i, 15)) --> [].
cbor_minor_value(16, val(i, 16)) --> [].
cbor_minor_value(17, val(i, 17)) --> [].
cbor_minor_value(18, val(i, 18)) --> [].
cbor_minor_value(19, val(i, 19)) --> [].
cbor_minor_value(20, val(i, 20)) --> [].
cbor_minor_value(21, val(i, 21)) --> [].
cbor_minor_value(22, val(i, 22)) --> [].
cbor_minor_value(23, val(i, 23)) --> [].
% NOTE: {true} makes it work for decoding, possibly a bug on scryer side?
cbor_minor_value(24, val(x1, V)) --> {true}, numbytes_number(1, V).
cbor_minor_value(25, val(x2, V)) --> {true}, numbytes_number(2, V).
cbor_minor_value(26, val(x4, V)) --> {true}, numbytes_number(4, V).
cbor_minor_value(27, val(x8, V)) --> {true}, numbytes_number(8, V).
cbor_minor_value(28, reserved(28)) --> [].
cbor_minor_value(29, reserved(29)) --> [].
cbor_minor_value(30, reserved(30)) --> [].
cbor_minor_value(31, indefinite) --> [].

cbor_item(X) -->
  cbor_major_value(Major, Value),
  cbor_major_value_x(Major, Value, X).

cbor_major_value_x(0, Value, X) --> cbor_0_value_x(Value, X).
cbor_major_value_x(1, Value, X) --> cbor_1_value_x(Value, X).
cbor_major_value_x(2, Value, X) --> cbor_2_value_x(Value, X).
cbor_major_value_x(3, Value, X) --> cbor_3_value_x(Value, X).
cbor_major_value_x(4, Value, X) --> cbor_4_value_x(Value, X).
cbor_major_value_x(5, Value, X) --> cbor_5_value_x(Value, X).
cbor_major_value_x(6, Value, X) --> cbor_6_value_x(Value, X).
cbor_major_value_x(7, Value, X) --> cbor_7_value_x(Value, X).

cbor_0_value_x(val(M, V), unsigned(M, V)) --> [].
% Not well-formed: 0x00 + 28 = 28
cbor_0_value_x(reserved(28), nwf(28)) --> [].
cbor_0_value_x(reserved(29), nwf(29)) --> [].
cbor_0_value_x(reserved(30), nwf(30)) --> [].
cbor_0_value_x(indefinite, nwf(31)) --> [].

cbor_1_value_x(val(M, V), negative(M, X)) --> { #X #< 0, #X #= \ #V, #X + #V #= -1 }.
% Not well-formed: 0x20 + 28 = 60
cbor_1_value_x(reserved(28), nwf(60)) --> [].
cbor_1_value_x(reserved(29), nwf(61)) --> [].
cbor_1_value_x(reserved(30), nwf(62)) --> [].
cbor_1_value_x(indefinite, nwf(63)) --> [].

cbor_2_value_x(val(M, V), bytes(len(M, V), X)) --> numberbytes_list(V, X).
% Not well-formed: 0x40 + 28 = 92
cbor_2_value_x(reserved(28), nwf(92)) --> [].
cbor_2_value_x(reserved(29), nwf(93)) --> [].
cbor_2_value_x(reserved(30), nwf(94)) --> [].
cbor_2_value_x(indefinite, bytes(*, X)) --> indefinite_bytes(X).

cbor_3_value_x(val(M, V), text(len(M, V), X)) --> numberbytes_text(V, X).
% Not well-formed: 0x60 + 28 = 124
cbor_3_value_x(reserved(28), nwf(124)) --> [].
cbor_3_value_x(reserved(29), nwf(125)) --> [].
cbor_3_value_x(reserved(30), nwf(126)) --> [].
cbor_3_value_x(indefinite, text(*, X)) --> indefinite_text(X).

cbor_4_value_x(val(M, V), array(len(M, V), X)) --> numberbytes_array(V, X).
% Not well-formed: 0x80 + 28 = 156
cbor_4_value_x(reserved(28), nwf(156)) --> [].
cbor_4_value_x(reserved(29), nwf(157)) --> [].
cbor_4_value_x(reserved(30), nwf(158)) --> [].
cbor_4_value_x(indefinite, array(*, X)) --> indefinite_array(X).

cbor_5_value_x(val(M, V), map(len(M, V), X)) --> numberbytes_map(V, X).
% Not well-formed: 0xa0 + 28 = 188
cbor_5_value_x(reserved(28), nwf(188)) --> [].
cbor_5_value_x(reserved(29), nwf(189)) --> [].
cbor_5_value_x(reserved(30), nwf(190)) --> [].
cbor_5_value_x(indefinite, map(*, X)) --> indefinite_map(X).

cbor_6_value_x(val(M, V), tag(tag(M, V), X)) --> cbor_item(X).
% Not well-formed: 0xc0 + 28 = 220
cbor_6_value_x(reserved(28), nwf(220)) --> [].
cbor_6_value_x(reserved(29), nwf(221)) --> [].
cbor_6_value_x(reserved(30), nwf(222)) --> [].
cbor_6_value_x(indefinite,   nwf(223)) --> [].

cbor_7_value_x(val(A, V), X) --> { simple_or_float(A, V, X) }.
% Not well-formed: 0xe0 + 28 = 252
cbor_7_value_x(reserved(28), nwf(252)) --> [].
cbor_7_value_x(reserved(29), nwf(253)) --> [].
cbor_7_value_x(reserved(30), nwf(254)) --> [].
cbor_7_value_x(indefinite,   break) --> [].

numbytes_number(1, X) --> byte(X).
numbytes_number(2, X) --> { N = 1, #X1_ #= #X1 << (8 * N), #X #= #X1_ \/ #X0, #X #= #X1_ xor #X0, #X1 #= #X >> (8 * N) }, numbytes_number(N, X1), numbytes_number(N, X0).
numbytes_number(4, X) --> { N = 2, #X1_ #= #X1 << (8 * N), #X #= #X1_ \/ #X0, #X #= #X1_ xor #X0, #X1 #= #X >> (8 * N) }, numbytes_number(N, X1), numbytes_number(N, X0).
numbytes_number(8, X) --> { N = 4, #X1_ #= #X1 << (8 * N), #X #= #X1_ \/ #X0, #X #= #X1_ xor #X0, #X1 #= #X >> (8 * N) }, numbytes_number(N, X1), numbytes_number(N, X0).

numberbytes_list(N, L) --> { length(L, N) }, seq(L).
numberbytes_text(N, T) --> numberbytes_list(N, L), { chars_utf8bytes(T, L) }.
numberbytes_array(N, A) --> { length(A, N) }, foldl(cbor_item, A).
numberbytes_map(N, M) --> { length(M, N) }, foldl(cbor_pair, M).

indefinite_bytes(X) --> cbor_item(Item), indefinite_help_(Item, X, bytes_uni, indefinite_bytes).
bytes_uni(unsigned(M, V), nwf(unsigned(M, V))).
bytes_uni(negative(M, V), nwf(negative(M, V))).
bytes_uni(text(    L, V), nwf(bytes(   L, V))).
bytes_uni(array(   L, V), nwf(array(   L, V))).
bytes_uni(map(     L, V), nwf(map(     L, V))).
bytes_uni(tag(     T, V), nwf(tag(     T, V))).
bytes_uni(simple(  M, V), nwf(simple(  M, V))).
bytes_uni(float(   S, V), nwf(float(   S, V))).
bytes_uni(nwf(Byte)     , nwf(nwf(Byte))).
bytes_uni(bytes(L, V), A) :- len_uni_(L, bytes(L, V), A).

indefinite_text(X) --> cbor_item(Item), indefinite_help_(Item, X, text_uni, indefinite_text).
text_uni(unsigned(M, V), nwf(unsigned(M, V))).
text_uni(negative(M, V), nwf(negative(M, V))).
text_uni(bytes(   L, V), nwf(bytes(   L, V))).
text_uni(array(   L, V), nwf(array(   L, V))).
text_uni(map(     L, V), nwf(map(     L, V))).
text_uni(tag(     T, V), nwf(tag(     T, V))).
text_uni(simple(  M, V), nwf(simple(  M, V))).
text_uni(float(   S, V), nwf(float(   S, V))).
text_uni(nwf(Byte)     , nwf(nwf(Byte))).
text_uni(text(L, V), A) :- len_uni_(L, text(L, V), A).

len_uni_(len(_, _), A, A).
len_uni_(*, A, nwf(A)).

indefinite_array(X) --> cbor_item(Item), indefinite_help_(Item, X, =, indefinite_array).

indefinite_map(X) --> cbor_item(Item), indefinite_help_(Item, X, map_uni(Val), indefinite_map_(Val)).
indefinite_map_(Val, X) --> cbor_item(Val), indefinite_map(X).
map_uni(Val, Key, Key-Val).

:- meta_predicate(indefinite_help_(?, ?, 2, 3, 5, ?, ?)).

indefinite_help_(unsigned(M, V), [Item | X], In_Out, DCG) --> { call(In_Out, unsigned(M, V), Item) }, call(DCG, X).
indefinite_help_(negative(M, V), [Item | X], In_Out, DCG) --> { call(In_Out, negative(M, V), Item) }, call(DCG, X).
indefinite_help_(bytes(   L, V), [Item | X], In_Out, DCG) --> { call(In_Out, bytes(   L, V), Item) }, call(DCG, X).
indefinite_help_(text(    L, V), [Item | X], In_Out, DCG) --> { call(In_Out, text(    L, V), Item) }, call(DCG, X).
indefinite_help_(array(   L, V), [Item | X], In_Out, DCG) --> { call(In_Out, array(   L, V), Item) }, call(DCG, X).
indefinite_help_(map(     L, V), [Item | X], In_Out, DCG) --> { call(In_Out, map(     L, V), Item) }, call(DCG, X).
indefinite_help_(tag(     T, V), [Item | X], In_Out, DCG) --> { call(In_Out, tag(     T, V), Item) }, call(DCG, X).
indefinite_help_(simple(  M, V), [Item | X], In_Out, DCG) --> { call(In_Out, simple(  M, V), Item) }, call(DCG, X).
indefinite_help_(float(   S, V), [Item | X], In_Out, DCG) --> { call(In_Out, float(   S, V), Item) }, call(DCG, X).
indefinite_help_(nwf(Byte)     , [Item | X], In_Out, DCG) --> { call(In_Out, nwf(Byte)     , Item) }, call(DCG, X).
indefinite_help_(break         , [], _, _) --> [].

simple_or_float(i, V, simple(i, V)) :- V in 0..23.
simple_or_float(x1, V, S) :-
  ( V in 0x00..0x1f, S = nwf(simple(x1, V))
  ; V in 0x20..0xff, S = simple(x1, V)
  ).
simple_or_float(x2, V, float(16, X)) :- size_number_float(x2, V, X).
simple_or_float(x3, V, float(32, X)) :- size_number_float(x3, V, X).
simple_or_float(x4, V, float(64, X)) :- size_number_float(x4, V, X).

number_float(_, F, F). % TODO
