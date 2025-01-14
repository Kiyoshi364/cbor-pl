:- module(cbor, [
  cbor_item//1, cbor//1
]).

:- use_module(library(dcgs), [seq//1]).
:- use_module(library(clpz), [
  (#<)/2, (#=)/2,
  op(700, xfx, #=),
  op(150, fx, #)
]).
:- use_module(library(lists), [length/2]).

:- initialization(assertz(clpz:monotonic)).

cbor(X) --> cbor_item(X).

header_major_minor(Header, Major, Minor) :-
  #Major #= #Header >> 5,
  #Minor #= #Header /\ 0x1f,
true.

cbor_major_minor(Major, Minor) -->
  [H],
  { header_major_minor(H, Major, Minor) }.

cbor_major_value(Major, Value) -->
  cbor_major_minor(Major, Minor),
  cbor_minor_value(Minor, Value).

cbor_minor_value( 0, val( 0)) --> [].
cbor_minor_value( 1, val( 1)) --> [].
cbor_minor_value( 2, val( 2)) --> [].
cbor_minor_value( 3, val( 3)) --> [].
cbor_minor_value( 4, val( 4)) --> [].
cbor_minor_value( 5, val( 5)) --> [].
cbor_minor_value( 6, val( 6)) --> [].
cbor_minor_value( 7, val( 7)) --> [].
cbor_minor_value( 8, val( 8)) --> [].
cbor_minor_value( 9, val( 9)) --> [].
cbor_minor_value(10, val(10)) --> [].
cbor_minor_value(11, val(11)) --> [].
cbor_minor_value(12, val(12)) --> [].
cbor_minor_value(13, val(13)) --> [].
cbor_minor_value(14, val(14)) --> [].
cbor_minor_value(15, val(15)) --> [].
cbor_minor_value(16, val(16)) --> [].
cbor_minor_value(17, val(17)) --> [].
cbor_minor_value(18, val(18)) --> [].
cbor_minor_value(19, val(19)) --> [].
cbor_minor_value(20, val(20)) --> [].
cbor_minor_value(21, val(21)) --> [].
cbor_minor_value(22, val(22)) --> [].
cbor_minor_value(23, val(23)) --> [].
cbor_minor_value(24, val( V)) --> numbytes_number(1, V).
cbor_minor_value(25, val( V)) --> numbytes_number(2, V).
cbor_minor_value(26, val( V)) --> numbytes_number(4, V).
cbor_minor_value(27, val( V)) --> numbytes_number(8, V).
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

cbor_0_value_x(val(V), uint(V)) --> [].
% Not well-formed: 0x00 + 28 = 28
cbor_0_value_x(reserved(28), nwf(28)) --> [].
cbor_0_value_x(reserved(29), nwf(29)) --> [].
cbor_0_value_x(reserved(30), nwf(30)) --> [].
cbor_0_value_x(indefinite, nwf(31)) --> [].

cbor_1_value_x(val(V), int(X)) --> { #X #= \ #V }.
% Not well-formed: 0x20 + 28 = 60
cbor_1_value_x(reserved(28), nwf(60)) --> [].
cbor_1_value_x(reserved(29), nwf(61)) --> [].
cbor_1_value_x(reserved(30), nwf(62)) --> [].
cbor_1_value_x(indefinite, nwf(63)) --> [].

cbor_2_value_x(val(V), bytes(X)) --> numberbytes_list(V, X).
% Not well-formed: 0x40 + 28 = 92
cbor_2_value_x(reserved(28), nwf(92)) --> [].
cbor_2_value_x(reserved(29), nwf(93)) --> [].
cbor_2_value_x(reserved(30), nwf(94)) --> [].
cbor_2_value_x(indefinite, not_implemented) --> []. % TODO

cbor_3_value_x(val(V), text(X)) --> numberbytes_text(V, X).
% Not well-formed: 0x60 + 28 = 124
cbor_3_value_x(reserved(28), nwf(124)) --> [].
cbor_3_value_x(reserved(29), nwf(125)) --> [].
cbor_3_value_x(reserved(30), nwf(126)) --> [].
cbor_3_value_x(indefinite, not_implemented) --> []. % TODO

numbytes_number(1, X) --> [X].
numbytes_number(2, X) --> { N = 1, #X #= (#X1 << (8 * #N)) \/ #X0 }, numbytes_number(N, X1), numbytes_number(N, X0).
numbytes_number(4, X) --> { N = 2, #X #= (#X1 << (8 * #N)) \/ #X0 }, numbytes_number(N, X1), numbytes_number(N, X0).
numbytes_number(8, X) --> { N = 4, #X #= (#X1 << (8 * #N)) \/ #X0 }, numbytes_number(N, X1), numbytes_number(N, X0).

numberbytes_list(N, L) --> { length(L, N) }, seq(L).

numberbytes_text(N, T) --> numberbytes_text(N, utf8_begin, T).
numberbytes_text(N0, S0, T) -->
  ( { #N0 #= 0, S0 = utf8_begin, T = [] }
  ; { 0 #< #N0, #N #= #N0 - 1 },
    [Byte],
    { utf8_state_byte_next(S0, Byte, S, T, T1) },
    numberbytes_text(N, S, T1)
  ).

% TODO: only implemented for simple case: ascii only
utf8_state_byte_next(utf8_begin, Byte, utf8_begin, [Char | T], T) :-
  #Byte #< 0x80,
  char_code(Char, Byte).
