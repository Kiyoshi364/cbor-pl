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
:- use_module(library(lists), [foldl/4, length/2]).

:- initialization(assertz(clpz:monotonic)).

cbor(X) --> cbor_item(X).

cbor_pair(K-V) --> cbor_item(K), cbor_item(V).

byte( X) :- X in 0x00..0xff.
short(X) :- X in 0x00..0xffff.
word( X) :- X in 0x00..0xffffffff.
quad( X) :- X in 0x00..0xffffffffffffffff.

byte( X) --> {  byte(X) }, [X].

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

cbor_0_value_x(val(_, V), unsigned(V)) --> [].
% Not well-formed: 0x00 + 28 = 28
cbor_0_value_x(reserved(28), nwf(28)) --> [].
cbor_0_value_x(reserved(29), nwf(29)) --> [].
cbor_0_value_x(reserved(30), nwf(30)) --> [].
cbor_0_value_x(indefinite, nwf(31)) --> [].

cbor_1_value_x(val(_, V), negative(X)) --> { #X #< 0, #X #= \ #V, #X + #V #= -1 }.
% Not well-formed: 0x20 + 28 = 60
cbor_1_value_x(reserved(28), nwf(60)) --> [].
cbor_1_value_x(reserved(29), nwf(61)) --> [].
cbor_1_value_x(reserved(30), nwf(62)) --> [].
cbor_1_value_x(indefinite, nwf(63)) --> [].

cbor_2_value_x(val(_, V), bytes(len(V), X)) --> numberbytes_list(V, X).
% Not well-formed: 0x40 + 28 = 92
cbor_2_value_x(reserved(28), nwf(92)) --> [].
cbor_2_value_x(reserved(29), nwf(93)) --> [].
cbor_2_value_x(reserved(30), nwf(94)) --> [].
cbor_2_value_x(indefinite, not_implemented) --> []. % TODO

cbor_3_value_x(val(_, V), text(len(V), X)) --> numberbytes_text(V, X).
% Not well-formed: 0x60 + 28 = 124
cbor_3_value_x(reserved(28), nwf(124)) --> [].
cbor_3_value_x(reserved(29), nwf(125)) --> [].
cbor_3_value_x(reserved(30), nwf(126)) --> [].
cbor_3_value_x(indefinite, not_implemented) --> []. % TODO

cbor_4_value_x(val(_, V), array(len(V), X)) --> numberbytes_array(V, X).
% Not well-formed: 0x80 + 28 = 156
cbor_4_value_x(reserved(28), nwf(156)) --> [].
cbor_4_value_x(reserved(29), nwf(157)) --> [].
cbor_4_value_x(reserved(30), nwf(158)) --> [].
cbor_4_value_x(indefinite, not_implemented) --> []. % TODO

cbor_5_value_x(val(_, V), map(len(V), X)) --> numberbytes_map(V, X).
% Not well-formed: 0xa0 + 28 = 188
cbor_5_value_x(reserved(28), nwf(188)) --> [].
cbor_5_value_x(reserved(29), nwf(189)) --> [].
cbor_5_value_x(reserved(30), nwf(190)) --> [].
cbor_5_value_x(indefinite, not_implemented) --> []. % TODO

cbor_6_value_x(val(_, V), tag(tag(V), X)) --> cbor_item(X).
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
cbor_7_value_x(indefinite,   nwf(255)) --> [].

numbytes_number(1, X) --> byte(X).
numbytes_number(2, X) --> { N = 1, #X1_ #= #X1 << (8 * N), #X #= #X1_ \/ #X0, #X #= #X1_ xor #X0, #X1 #= #X >> (8 * N) }, numbytes_number(N, X1), numbytes_number(N, X0).
numbytes_number(4, X) --> { N = 2, #X1_ #= #X1 << (8 * N), #X #= #X1_ \/ #X0, #X #= #X1_ xor #X0, #X1 #= #X >> (8 * N) }, numbytes_number(N, X1), numbytes_number(N, X0).
numbytes_number(8, X) --> { N = 4, #X1_ #= #X1 << (8 * N), #X #= #X1_ \/ #X0, #X #= #X1_ xor #X0, #X1 #= #X >> (8 * N) }, numbytes_number(N, X1), numbytes_number(N, X0).

numberbytes_list(N, L) --> { length(L, N) }, seq(L).
numberbytes_array(N, A) --> { length(A, N) }, foldl(cbor_item, A).
numberbytes_map(N, M) --> { length(M, N) }, foldl(cbor_pair, M).

numberbytes_text(N, T) --> numberbytes_text(N, utf8_begin, T).
numberbytes_text(N0, S0, T) -->
  ( { #N0 #= 0, S0 = utf8_begin, T = [] }
  ; { 0 #< #N0, #N #= #N0 - 1 },
    byte(Byte),
    { utf8_state_byte_next(S0, Byte, S, T, T1) },
    numberbytes_text(N, S, T1)
  ).

% TODO: only implemented for simple case: ascii only
utf8_state_byte_next(utf8_begin, Byte, utf8_begin, [Char | T], T) :-
  #Byte #< 0x80,
  char_code(Char, Byte).

simple_or_float(i, V, simple(V)) :- V in 0..23.
simple_or_float(x1, V, simple(V)) :- V in 0x20..0xff.
simple_or_float(x2, V, float(16, X)) :- size_number_float(x2, V, X).
simple_or_float(x3, V, float(32, X)) :- size_number_float(x3, V, X).
simple_or_float(x4, V, float(64, X)) :- size_number_float(x4, V, X).

number_float(_, F, F). % TODO
