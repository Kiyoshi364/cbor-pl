/**
  Consider reading documentation for cbor/1 before
  using the main predicate cbor_item//2.
*/
:- module(cbor, [
  cbor/1,
  cbor_item//1
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
:- use_module(library(lists), [foldl/4, maplist/2, length/2]).
:- use_module(library(dif), [dif/2]).

%% cbor(+Item) is semidet. % doc(cbor/1).
%% cbor(?Item) is nondet.
%
%  Is true if cbor_item(Item) describes
%  a well-formed CBOR encoded byte list.
%  In more pratical terms,
%  if `cbor(Item)` then `phrase(cbor_item(Item), Bytes)` is true
%  and `Bytes` is a list of bytes which
%  is a well-formed CBOR and encodes `Item`.
%
%  Each clause maps a functor of a CBOR Item
%  to one Major Type from CBOR specification.
%  The exhaustive list:
%    * `unsigned(_, _)` maps to Major Type 0 (unsigned integer)
%    * `negative(_, _)` maps to Major Type 1 (negative integer)
%    * `bytes(_, _)`    maps to Major Type 2 (byte string)
%    * `text(_, _)`     maps to Major Type 3 (text string (utf8))
%    * `array(_, _)`    maps to Major Type 4 (array of items)
%    * `map(_, _)`      maps to Major Type 5 (map of pairs of item)
%    * `tag(_, _, _)`   maps to Major Type 6 (tagged item)
%    * `simple(_, _)`   maps to Major Type 7 (simple values)
%    * `float(_, _)`    maps to Major Type 7 (floating point numbers)
%
%  The predicate `cbor/1` is provided for both:
%    1. checking/completing/generating a well-formed CBOR Item.
%    2. documenting which CBOR Items `cbor_item//1` accepts.
%      Note that `cbor_item//1` also describes not well-formed CBOR Items,
%      in order to support reading partial information from
%      a not well-formed CBOR,
%      for more information see documentation for `cbor_item//1`.
%
%  The representation used in this library for a CBOR item
%  maps one-to-one to a byte encoding of certain CBOR Value.
%  A CBOR Value may have multiple byte representations,
%  for instance the value 1
%  may be represented in any of the following ways:
%    * [0x01]
%    * [0x18, 0x01]
%    * [0x19, 0x00, 0x01]
%    * [0x1a, 0x00, 0x00, 0x00, 0x01]
%    * [0x1b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]
%  This library uses one representation for each byte encoding.
%
%  For further describing the representation
%  we will use the predicate `place_value/2`.
%  The predicate `place_value(P, V)` is true
%  when the value `V` is possible to be held
%  in the place `P`.
%  `V` is the value of a major type,
%  it may be part or the entirety of the value of the item
%  (for unsigned, negative, tag and simple/float)
%  or some length used to find the value of the item
%  (for byte string, byte string, array and map).
%  `P` indicates where `V` lies in the byte encoding.
%  See documentation for `place_value/2` for more info.
%
%  Another important functor for the representation
%  is the length indicator.
%  It is either `len(P, N)` or `*` (the atom asterisk).
%  In the first case `len(P, N)`,
%  the length is `N` and it resides at place `P`,
%  also `place_value(P, N)` must be true.
%  In the second case `*`, the length is indefinite.
%
%  # Major 0 -- Unsigned
%
%  It is represented by `unsigned(P, X)`,
%  `X` is the unsigned number itself and
%  `P` is where `X` resides.
cbor(unsigned(P, X)) :- place_value(P, X).
%
%  # Major 1 -- Negative
%
%  It is represented by `negative(P, X)`,
%  `X` is the negative number itself and
%  `P` is where `X` resides.
%  `V` (in the implementation) is the value
%  in the byte representation.
cbor(negative(P, X)) :-
  #X #< 0,
  #X + #V #= -1,
  place_value(P, V).
%
%  # Major 2 -- Byte String
%
%  It is represented by `bytes(L, X)`.
%  `L` is a length indicator.
%  If `L = len(P, N)`, then
%  `X` is a list of bytes with length `N`.
%  Otherwise, `L = *` and
%  `X` is a list of Major 2 (byte string) items,
%  and each item has definite length (`L = len(P, N)`).
%  See `definite_bytes/1` documentation.
%
%  The following definition is similar to:
%  ```prolog
%    cbor(bytes(len(P, N), X)) :- definite_bytes(bytes(len(P, N), X)).
%    cbor(bytes(*, X)) :- maplist(definite_bytes, X).
%  ```
%  but it puts emphasis on first argument indexing.
cbor(bytes(L, X)) :- cbor_bytes(L, X).
%
%  # Major 3 -- Text String (UTF-8 encoded)
%
%  It is represented by `text(L, X)`.
%  `L` is a length indicator.
%  If `L = len(P, N)`, then
%  `X` is a list of char and
%  and its underlining represtation uses `N` bytes.
%  Otherwise, `L = *` and
%  `X` is a list of Major 3 (text string) items,
%  and each item has definite length (`L = len(P, N)`).
%  See `definite_text/1` documentation.
%
%  The following definition is similar to:
%  ```prolog
%    cbor(text(len(P, N), X)) :- definite_text(text(len(P, N), X)).
%    cbor(text(*, X)) :- maplist(definite_text, X).
%  ```
%  but it puts emphasis on first argument indexing.
cbor(text(L, X)) :- cbor_text(L, X).
%
%  # Major 4 -- Array
%
%  It is represented by `array(L, X)`.
%  `L` is a length indicator and
%  `X` is a list of cbor items.
cbor(array(L, X)) :- lengthindicator_length(L, N), length(X, N), maplist(cbor, X).
%
%  # Major 5 -- Map
%
%  It is represented by `map(L, X)`.
%  `L` is a length indicator and
%  `X` is a list of pairs of cbor items,
%  in another words, each item is of the form `Key-Value` and
%  both `Key` and `Value` are cbor items.
cbor(map(L, X)) :- lengthindicator_length(L, N), length(X, N), maplist(pair_of_cbor, X).
%
%  # Major 6 -- Tagged Item
%
%  It is represented by `tag(P, T, X)`.
%  `T` is the tag residing in place `P` and
%  `X` is the tagged item, a cbor item.
cbor(tag(P, T, X)) :- place_value(P, T), cbor(X).
%
%  # Major 7 -- Simple Values
%
%  It is represented by `simple(P, X)`.
%  `X` is the simple value residing in place `P`.
%  In this case, `P` may only be `i` or `x1`.
%
%  The following definition is similar to:
%  ```prolog
%    cbor(simple(i, X)) :- place_value(i, X).
%    cbor(simple(x1, X)) :- place_value(x1, X).
%  ```
%  but it puts emphasis on first argument indexing.
cbor(simple(P, X)) :- cbor_simple(P, X).
%
%  # Major 7 -- Floating Point Numbers
%
%  WARNING: `simple_value_float/3` is NOT IMPLEMENTED
%    it currently unifies the second and third arguments.
%
%  It is represented by `float(P, X)`.
%  `X` is the floating point number residing in place `P`.
%  In this case, `P` may only be `x2`, `x4` or `x8`.
%  `P` carries the byte-width information of the float.
%  The exhaustive list:
%    * `x2` means half   precision float (float16) (2 bytes),
%    * `x4` means single precision float (float32) (4 bytes),
%    * `x8` means double precision float (float64) (8 bytes),
%  See documentation for `size_value_float/3` for more info.
%
%  The following definition is similar to:
%  ```prolog
%    cbor(float(x2, X)) :- size_value_float(x2, _, X).
%    cbor(float(x4, X)) :- size_value_float(x4, _, X).
%    cbor(float(x8, X)) :- size_value_float(x8, _, X).
%  ```
%  but it puts emphasis on first argument indexing.
cbor(float(P, X)) :- cbor_float(P, X).

%% place_value(+P, ?V) is semidet. % doc(place_value/2).
%% place_value(?P, +V) is nondet.
%
%  `P` is one of the atoms: `i`, `x1`, `x2`, `x4`, `x8`.
%  * `i` means `V` fits in the head;
%  * `x1` means `V` fits in the next 1 byte;
%  * `x2` means `V` fits in the next 2 bytes;
%  * `x4` means `V` fits in the next 4 bytes;
%  * `x8` means `V` fits in the next 8 bytes.
place_value(i , V) :- V in 00..23.
place_value(x1, V) :- byte(V).
place_value(x2, V) :- short(V).
place_value(x4, V) :- word(V).
place_value(x8, V) :- quad(V).

%% lengthindicator_length(+L, ?N) is semidet. % doc(lengthindicator_length/2).
%% lengthindicator_length(?L, +N) is nondet.
%
%  `L` is either `len(P, N)` or `*` (the atom asterisk).
%  In the first case `len(P, N)`,
%  the length is `N` and it resides at place `P`,
%  also `place_value(P, N)` must be true.
%  In the second case `*`, the length is indefinite.
lengthindicator_length(len(P, N), N) :- place_value(P, N).
lengthindicator_length(*, N) :- byte(N).

%% definite_bytes(+Bytes) is semidet. % doc(definite_bytes/1).
%% definite_bytes(?Bytes) is nondet.
%
%  `Bytes` is `bytes(len(P, N), X)`,
%  `X` is a list of bytes with length `N`,
%  `N` is residing at place `P`,
%  also `place_value(P, N)` must be true.
definite_bytes(bytes(len(P, N), X)) :- place_value(P, N), length(X, N), maplist(byte, X).

cbor_bytes(len(P, N), X) :- place_value(P, N), length(X, N), maplist(byte, X).
cbor_bytes(*, X) :- maplist(definite_bytes, X).

%% definite_text(+Text) is semidet. % doc(definite_text/1).
%% definite_text(?Text) is nondet.
%
%  `Text` is `text(len(P, N), X)`,
%  `X` is a list of char
%  and its underlining represtation uses `N` bytes,
%  `N` is residing at place `P`,
%  also `place_value(P, N)` must be true.
definite_text(text(len(P, N), X)) :- place_value(P, N), chars_utf8bytes(X, L), length(L, N).

cbor_text(len(P, N), X) :- place_value(P, N), chars_utf8bytes(X, L), length(L, N).
cbor_text(*, X) :- maplist(definite_text, X).

pair_of_cbor(Key-Value) :- cbor(Key), cbor(Value).

cbor_simple(i , X) :- place_value(i , X).
cbor_simple(x1, X) :- place_value(x1, X).

cbor_float(x2, X) :- size_value_float(x2, _, X).
cbor_float(x4, X) :- size_value_float(x4, _, X).
cbor_float(x8, X) :- size_value_float(x8, _, X).

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
% NOTE: {true} makes it work for encoding, possibly a bug on scryer side?
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

cbor_0_value_x(val(P, V), unsigned(P, V)) --> [].
% Not well-formed: 0x00 + 28 = 28
cbor_0_value_x(reserved(28), nwf(28)) --> [].
cbor_0_value_x(reserved(29), nwf(29)) --> [].
cbor_0_value_x(reserved(30), nwf(30)) --> [].
cbor_0_value_x(indefinite, nwf(31)) --> [].

cbor_1_value_x(val(P, V), negative(P, X)) --> { #X #< 0, #X + #V #= -1 }.
% Not well-formed: 0x20 + 28 = 60
cbor_1_value_x(reserved(28), nwf(60)) --> [].
cbor_1_value_x(reserved(29), nwf(61)) --> [].
cbor_1_value_x(reserved(30), nwf(62)) --> [].
cbor_1_value_x(indefinite, nwf(63)) --> [].

cbor_2_value_x(val(P, V), bytes(len(P, V), X)) --> numberbytes_list(V, X).
% Not well-formed: 0x40 + 28 = 92
cbor_2_value_x(reserved(28), nwf(92)) --> [].
cbor_2_value_x(reserved(29), nwf(93)) --> [].
cbor_2_value_x(reserved(30), nwf(94)) --> [].
cbor_2_value_x(indefinite, bytes(*, X)) --> indefinite_bytes(X).

cbor_3_value_x(val(P, V), text(len(P, V), X)) --> numberbytes_text(V, X).
% Not well-formed: 0x60 + 28 = 124
cbor_3_value_x(reserved(28), nwf(124)) --> [].
cbor_3_value_x(reserved(29), nwf(125)) --> [].
cbor_3_value_x(reserved(30), nwf(126)) --> [].
cbor_3_value_x(indefinite, text(*, X)) --> indefinite_text(X).

cbor_4_value_x(val(P, V), array(len(P, V), X)) --> numberbytes_array(V, X).
% Not well-formed: 0x80 + 28 = 156
cbor_4_value_x(reserved(28), nwf(156)) --> [].
cbor_4_value_x(reserved(29), nwf(157)) --> [].
cbor_4_value_x(reserved(30), nwf(158)) --> [].
cbor_4_value_x(indefinite, array(*, X)) --> indefinite_array(X).

cbor_5_value_x(val(P, V), map(len(P, V), X)) --> numberbytes_map(V, X).
% Not well-formed: 0xa0 + 28 = 188
cbor_5_value_x(reserved(28), nwf(188)) --> [].
cbor_5_value_x(reserved(29), nwf(189)) --> [].
cbor_5_value_x(reserved(30), nwf(190)) --> [].
cbor_5_value_x(indefinite, map(*, X)) --> indefinite_map(X).

cbor_6_value_x(val(P, V), tag(P, V, X)) --> cbor_item(X).
% Not well-formed: 0xc0 + 28 = 220
cbor_6_value_x(reserved(28), nwf(220)) --> [].
cbor_6_value_x(reserved(29), nwf(221)) --> [].
cbor_6_value_x(reserved(30), nwf(222)) --> [].
cbor_6_value_x(indefinite,   nwf(223)) --> [].

cbor_7_value_x(val(P, V), X) --> { simple_or_float(P, V, X) }.
% Not well-formed: 0xe0 + 28 = 252
cbor_7_value_x(reserved(28), nwf(252)) --> [].
cbor_7_value_x(reserved(29), nwf(253)) --> [].
cbor_7_value_x(reserved(30), nwf(254)) --> [].
cbor_7_value_x(indefinite,   break) --> [].

numbytes_number(1, X) --> byte(X).
numbytes_number(2, X) --> { N = 1, #X1_ #= #X1 << (8 * N), #X #= #X1_ \/ #X0, #X #= #X1_ xor #X0, #X1 #= #X >> (8 * N) }, numbytes_number(N, X1), numbytes_number(N, X0).
numbytes_number(4, X) --> { N = 2, #X1_ #= #X1 << (8 * N), #X #= #X1_ \/ #X0, #X #= #X1_ xor #X0, #X1 #= #X >> (8 * N) }, numbytes_number(N, X1), numbytes_number(N, X0).
numbytes_number(8, X) --> { N = 4, #X1_ #= #X1 * (2 ^ (8 * N)), #X #= #X1_ \/ #X0, #X #= #X1_ xor #X0, #X1 #= #X >> (8 * N) }, numbytes_number(N, X1), numbytes_number(N, X0).
% NOTE: `X * (2 ^ (8 * 4))` and `X << (8 * 4)` have the same result,
%  unless if `X in 0x80000000..0xffffffff`.
%  This happens, because the result is interpreted as an negative 64bit number.


numberbytes_list(N, L) --> { length(L, N) }, seq(L).
numberbytes_text(N, T) --> numberbytes_list(N, L), { chars_utf8bytes(T, L) }.
numberbytes_array(N, A) --> { length(A, N) }, foldl(cbor_item, A).
numberbytes_map(N, M) --> { length(M, N) }, foldl(cbor_pair, M).

indefinite_bytes(X) --> indefinite_help_(X, bytes_uni, indefinite_bytes).
bytes_uni(bytes(L, V), bytes(L, V)) :- L = len(_, _).
bytes_uni(nwf(A), A).

indefinite_text(X) --> indefinite_help_(X, text_uni, indefinite_text).
text_uni(text(L, V), text(L, V)) :- L = len(_, _).
text_uni(nwf(A), A).

indefinite_array(X) --> indefinite_help_(X, =, indefinite_array).

indefinite_map(X) --> indefinite_help_(X, map_uni(Val), indefinite_map_(Val)).
indefinite_map_(Val, X) --> cbor_item(Val), indefinite_map(X).
map_uni(Val, Key-Val, Key).

:- meta_predicate(indefinite_help_(?, 2, 3, ?, ?)).

indefinite_help_([], _, _) --> cbor_item(break).
indefinite_help_([V | X], Out_In, DCG) --> { call(cbor:Out_In, V, Item), dif(Item, break) }, cbor_item(Item), call(cbor:DCG, X).

simple_or_float(i, V, simple(i, V)) :- V in 0..23.
simple_or_float(x1, V, S) :-
  ( V in 0x00..0x1f, S = nwf(simple(x1, V))
  ; V in 0x20..0xff, S = simple(x1, V)
  ).
simple_or_float(x2, V, float(x2, X)) :- size_value_float(x2, V, X).
simple_or_float(x4, V, float(x4, X)) :- size_value_float(x4, V, X).
simple_or_float(x8, V, float(x8, X)) :- size_value_float(x8, V, X).

%% size_value_float(S, I, F). % doc(size_value_float/3).
%
%  WARNING: it is NOT IMPLEMENTED
%    it currently unifies `I` and `F`.
%
%  Is true if the float `F` has
%  an byte representation `I` using size `S`.
%  `S` is one of:
%    * `x2` (2 bytes)
%    * `x4` (4 bytes)
%    * `x8` (8 bytes)
size_value_float(_, F, F). % TODO
