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

cbor_major_minor(Major, Minor) -->
  [H],
  { #Major #= #H >> 5,
    #Minor #= #H /\ 0x1f
  }.

cbor_item(X) -->
  cbor_major_minor(Major, Minor),
  cbor_major_minor_x(Major, Minor, X).

cbor_major_minor_x(0, Minor, X) --> cbor_0_minor_x(Minor, X).
cbor_major_minor_x(1, Minor, X) --> cbor_1_minor_x(Minor, X).
cbor_major_minor_x(2, Minor, X) --> cbor_2_minor_x(Minor, X).
cbor_major_minor_x(3, Minor, X) --> cbor_3_minor_x(Minor, X).
cbor_major_minor_x(4, Minor, X) --> cbor_4_minor_x(Minor, X).
cbor_major_minor_x(5, Minor, X) --> cbor_5_minor_x(Minor, X).
cbor_major_minor_x(6, Minor, X) --> cbor_6_minor_x(Minor, X).
cbor_major_minor_x(7, Minor, X) --> cbor_7_minor_x(Minor, X).

cbor_0_minor_x( 0, uint( 0)) --> [].
cbor_0_minor_x( 1, uint( 1)) --> [].
cbor_0_minor_x( 2, uint( 2)) --> [].
cbor_0_minor_x( 3, uint( 3)) --> [].
cbor_0_minor_x( 4, uint( 4)) --> [].
cbor_0_minor_x( 5, uint( 5)) --> [].
cbor_0_minor_x( 6, uint( 6)) --> [].
cbor_0_minor_x( 7, uint( 7)) --> [].
cbor_0_minor_x( 8, uint( 8)) --> [].
cbor_0_minor_x( 9, uint( 9)) --> [].
cbor_0_minor_x(10, uint(10)) --> [].
cbor_0_minor_x(11, uint(11)) --> [].
cbor_0_minor_x(12, uint(12)) --> [].
cbor_0_minor_x(13, uint(13)) --> [].
cbor_0_minor_x(14, uint(14)) --> [].
cbor_0_minor_x(15, uint(15)) --> [].
cbor_0_minor_x(16, uint(16)) --> [].
cbor_0_minor_x(17, uint(17)) --> [].
cbor_0_minor_x(18, uint(18)) --> [].
cbor_0_minor_x(19, uint(19)) --> [].
cbor_0_minor_x(20, uint(20)) --> [].
cbor_0_minor_x(21, uint(21)) --> [].
cbor_0_minor_x(22, uint(22)) --> [].
cbor_0_minor_x(23, uint(23)) --> [].
cbor_0_minor_x(24, uint( X)) --> numbytes_number(1, X).
cbor_0_minor_x(25, uint( X)) --> numbytes_number(2, X).
cbor_0_minor_x(26, uint( X)) --> numbytes_number(4, X).
cbor_0_minor_x(27, uint( X)) --> numbytes_number(8, X).
% Not well-formed: 0x00 + 28 = 28
cbor_0_minor_x(28, nwf(28)) --> [].
cbor_0_minor_x(29, nwf(29)) --> [].
cbor_0_minor_x(30, nwf(30)) --> [].
cbor_0_minor_x(31, nwf(31)) --> [].

cbor_1_minor_x( 0, int( -1)) --> [].
cbor_1_minor_x( 1, int( -2)) --> [].
cbor_1_minor_x( 2, int( -3)) --> [].
cbor_1_minor_x( 3, int( -4)) --> [].
cbor_1_minor_x( 4, int( -5)) --> [].
cbor_1_minor_x( 5, int( -6)) --> [].
cbor_1_minor_x( 6, int( -7)) --> [].
cbor_1_minor_x( 7, int( -8)) --> [].
cbor_1_minor_x( 8, int( -9)) --> [].
cbor_1_minor_x( 9, int(-10)) --> [].
cbor_1_minor_x(10, int(-11)) --> [].
cbor_1_minor_x(11, int(-12)) --> [].
cbor_1_minor_x(12, int(-13)) --> [].
cbor_1_minor_x(13, int(-14)) --> [].
cbor_1_minor_x(14, int(-15)) --> [].
cbor_1_minor_x(15, int(-16)) --> [].
cbor_1_minor_x(16, int(-17)) --> [].
cbor_1_minor_x(17, int(-18)) --> [].
cbor_1_minor_x(18, int(-19)) --> [].
cbor_1_minor_x(19, int(-20)) --> [].
cbor_1_minor_x(20, int(-21)) --> [].
cbor_1_minor_x(21, int(-22)) --> [].
cbor_1_minor_x(22, int(-23)) --> [].
cbor_1_minor_x(23, int(-24)) --> [].
cbor_1_minor_x(24, int(  X)) --> { #X #= \ #X0 }, numbytes_number(1, X0).
cbor_1_minor_x(25, int(  X)) --> { #X #= \ #X0 }, numbytes_number(2, X0).
cbor_1_minor_x(26, int(  X)) --> { #X #= \ #X0 }, numbytes_number(4, X0).
cbor_1_minor_x(27, int(  X)) --> { #X #= \ #X0 }, numbytes_number(8, X0).
% Not well-formed: 0x20 + 28 = 60
cbor_1_minor_x(28, nwf(60)) --> [].
cbor_1_minor_x(29, nwf(61)) --> [].
cbor_1_minor_x(30, nwf(62)) --> [].
cbor_1_minor_x(31, nwf(63)) --> [].

cbor_2_minor_x( 0, bytes(X)) --> numbytes_list( 0, X).
cbor_2_minor_x( 1, bytes(X)) --> numbytes_list( 1, X).
cbor_2_minor_x( 2, bytes(X)) --> numbytes_list( 2, X).
cbor_2_minor_x( 3, bytes(X)) --> numbytes_list( 3, X).
cbor_2_minor_x( 4, bytes(X)) --> numbytes_list( 4, X).
cbor_2_minor_x( 5, bytes(X)) --> numbytes_list( 5, X).
cbor_2_minor_x( 6, bytes(X)) --> numbytes_list( 6, X).
cbor_2_minor_x( 7, bytes(X)) --> numbytes_list( 7, X).
cbor_2_minor_x( 8, bytes(X)) --> numbytes_list( 8, X).
cbor_2_minor_x( 9, bytes(X)) --> numbytes_list( 9, X).
cbor_2_minor_x(10, bytes(X)) --> numbytes_list(10, X).
cbor_2_minor_x(11, bytes(X)) --> numbytes_list(11, X).
cbor_2_minor_x(12, bytes(X)) --> numbytes_list(12, X).
cbor_2_minor_x(13, bytes(X)) --> numbytes_list(13, X).
cbor_2_minor_x(14, bytes(X)) --> numbytes_list(14, X).
cbor_2_minor_x(15, bytes(X)) --> numbytes_list(15, X).
cbor_2_minor_x(16, bytes(X)) --> numbytes_list(16, X).
cbor_2_minor_x(17, bytes(X)) --> numbytes_list(17, X).
cbor_2_minor_x(18, bytes(X)) --> numbytes_list(18, X).
cbor_2_minor_x(19, bytes(X)) --> numbytes_list(19, X).
cbor_2_minor_x(20, bytes(X)) --> numbytes_list(20, X).
cbor_2_minor_x(21, bytes(X)) --> numbytes_list(21, X).
cbor_2_minor_x(22, bytes(X)) --> numbytes_list(22, X).
cbor_2_minor_x(23, bytes(X)) --> numbytes_list(23, X).
cbor_2_minor_x(24, bytes(X)) --> numbytes_number(1, N), numberbytes_list(N, X).
cbor_2_minor_x(25, bytes(X)) --> numbytes_number(2, N), numberbytes_list(N, X).
cbor_2_minor_x(26, bytes(X)) --> numbytes_number(4, N), numberbytes_list(N, X).
cbor_2_minor_x(27, bytes(X)) --> numbytes_number(8, N), numberbytes_list(N, X).
% Not well-formed: 0x40 + 28 = 92
cbor_2_minor_x(28, nwf(92)) --> [].
cbor_2_minor_x(29, nwf(93)) --> [].
cbor_2_minor_x(30, nwf(94)) --> [].
cbor_2_minor_x(31, not_implemented) --> []. % TODO

cbor_3_minor_x( 0, text(X)) --> numbytes_text( 0, X).
cbor_3_minor_x( 1, text(X)) --> numbytes_text( 1, X).
cbor_3_minor_x( 2, text(X)) --> numbytes_text( 2, X).
cbor_3_minor_x( 3, text(X)) --> numbytes_text( 3, X).
cbor_3_minor_x( 4, text(X)) --> numbytes_text( 4, X).
cbor_3_minor_x( 5, text(X)) --> numbytes_text( 5, X).
cbor_3_minor_x( 6, text(X)) --> numbytes_text( 6, X).
cbor_3_minor_x( 7, text(X)) --> numbytes_text( 7, X).
cbor_3_minor_x( 8, text(X)) --> numbytes_text( 8, X).
cbor_3_minor_x( 9, text(X)) --> numbytes_text( 9, X).
cbor_3_minor_x(10, text(X)) --> numbytes_text(10, X).
cbor_3_minor_x(11, text(X)) --> numbytes_text(11, X).
cbor_3_minor_x(12, text(X)) --> numbytes_text(12, X).
cbor_3_minor_x(13, text(X)) --> numbytes_text(13, X).
cbor_3_minor_x(14, text(X)) --> numbytes_text(14, X).
cbor_3_minor_x(15, text(X)) --> numbytes_text(15, X).
cbor_3_minor_x(16, text(X)) --> numbytes_text(16, X).
cbor_3_minor_x(17, text(X)) --> numbytes_text(17, X).
cbor_3_minor_x(18, text(X)) --> numbytes_text(18, X).
cbor_3_minor_x(19, text(X)) --> numbytes_text(19, X).
cbor_3_minor_x(20, text(X)) --> numbytes_text(20, X).
cbor_3_minor_x(21, text(X)) --> numbytes_text(21, X).
cbor_3_minor_x(22, text(X)) --> numbytes_text(22, X).
cbor_3_minor_x(23, text(X)) --> numbytes_text(23, X).
cbor_3_minor_x(24, text(X)) --> numbytes_number(1, N), numberbytes_text(N, X).
cbor_3_minor_x(25, text(X)) --> numbytes_number(2, N), numberbytes_text(N, X).
cbor_3_minor_x(26, text(X)) --> numbytes_number(4, N), numberbytes_text(N, X).
cbor_3_minor_x(27, text(X)) --> numbytes_number(8, N), numberbytes_text(N, X).
% Not well-formed: 0x60 + 28 = 124
cbor_3_value_x(28, nwf(124)) --> [].
cbor_3_value_x(29, nwf(125)) --> [].
cbor_3_value_x(30, nwf(126)) --> [].
cbor_3_value_x(31, not_implemented) --> []. % TODO

numbytes_number(N, X) --> { number_peano(N, P) }, peanobytes_number(P, X).
numbytes_list(N, L) --> numberbytes_list(N, L).
numbytes_text(N, X) --> { number_peano(N, P) }, peanobytes_text(P, X).

peanobytes_number(P, X) --> peanobytes_number(P, 0, X).
peanobytes_number([], X, X) --> [].
peanobytes_number([_ | P], X0, X) -->
  [Byte],
  { #X1 #= (#X0 << 8) \/ #Byte },
  peanobytes_number(P, X1, X).

numberbytes_list(N, L) --> { length(L, N) }, seq(L).

numberbytes_text(N, T) --> numberbytes_text(N, utf8_begin, T).
numberbytes_text(N0, S0, T) -->
  ( { #N0 #= 0, S0 = utf8_begin, T = [] }
  ; { 0 #< #N0, #N #= #N0 - 1 },
    [Byte],
    { utf8_state_byte_next(S0, Byte, S, T, T1) },
    numberbytes_text(N, S, T1)
  ).

peanobytes_text(P, T) --> peanobytes_text(P, utf8_begin, T).
peanobytes_text([], utf8_begin, []) --> [].
peanobytes_text([_ | P], S0, T) -->
  [Byte],
  { utf8_state_byte_next(S0, Byte, S, T, T1) },
  peanobytes_text(P, S, T1).

% TODO: only implemented for simple case: ascii only
utf8_state_byte_next(utf8_begin, Byte, utf8_begin, [Char | T], T) :-
  #Byte #< 0x80,
  char_code(Char, Byte).

number_peano( 0, "").
number_peano( 1, "s").
number_peano( 2, "ss").
number_peano( 3, "sss").
number_peano( 4, "ssss").
number_peano( 5, "sssss").
number_peano( 6, "ssssss").
number_peano( 7, "sssssss").
number_peano( 8, "ssssssss").
number_peano( 9, "sssssssss").
number_peano(10, "ssssssssss").
number_peano(11, "sssssssssss").
number_peano(12, "ssssssssssss").
number_peano(13, "sssssssssssss").
number_peano(14, "ssssssssssssss").
number_peano(15, "sssssssssssssss").
number_peano(16, "ssssssssssssssss").
number_peano(17, "sssssssssssssssss").
number_peano(18, "ssssssssssssssssss").
number_peano(19, "sssssssssssssssssss").
number_peano(20, "ssssssssssssssssssss").
number_peano(21, "sssssssssssssssssssss").
number_peano(22, "ssssssssssssssssssssss").
number_peano(23, "sssssssssssssssssssssss").
