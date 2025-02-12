/**
  A simple library for quickly lifting one out of raw bytes
  without losing any possible representation for CBOR Values.
  Possibly a good foundation for building
  a higher-level CBOR encoder/decoder.

  Consider reading documentation for `cbor/1` before
  using the main predicate `cbor_item//1`.
  This library is tested for [Scryer Prolog](https://scryer.pl)
  version 8ac663d (after v0.9.4).
*/
:- module(cbor, [
  % Documentation predicates
  cbor/1,
  cbor_bytes/2,
  definite_bytes/1,
  cbor_text/2,
  definite_text/1,
  cbor_array/2,
  cbor_map/2,
  pair_of_cbor/1,
  cbor_tag/2,
  cbor_simple/2,
  cbor_float/2,
  place_value/2,
  lengthindicator_length/2,

  nwf_cbor/1,

  % Public DCGs
  cbor_item//1,
  cbor_item//2
]).

/**
  Possibly useful links:
  * [RFC8948](https://www.rfc-editor.org/rfc/rfc8949.html)
  * [CBOR Wikipedia](https://en.wikipedia.org/wiki/CBOR)
  * [CBOR.io](https://cbor.io/)
  * [CBOR Visualizer](https://cbor.me/)

  Other Specifications, RFCs, reserved values and stuff:
  * [Old CBOR Specification (RFC7049)](https://www.rfc-editor.org/info/rfc7049)
  * [IANA CBOR Simple Values](https://www.iana.org/assignments/cbor-simple-values/cbor-simple-values.xhtml)
  * [IANA CBOR Tags](https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml)
  * [CBOR Data Definition Language (RFC8610)](https://www.rfc-editor.org/rfc/rfc8610.html)
*/

:- use_module(library(dcgs), []).
:- use_module(library(clpz), [
  (#<)/2, (#=)/2, (in)/2,
  op(700, xfx, #<),
  op(700, xfx, #=),
  op(700, xfx, in),
  op(450, xfx, ..),
  op(150, fx, #)
]).
:- use_module(library(charsio), [chars_utf8bytes/2]).
:- use_module(library(freeze), [freeze/2]).
:- use_module(library(error), [must_be/2, instantiation_error/1, domain_error/3]).
:- use_module(library(lists), [member/2, foldl/4, maplist/2, length/2]).
:- use_module(library(reif), [if_/3, dif/3]).

%% cbor(+Item) is semidet. % doc(cbor/1).
%% cbor(?Item) is nondet.
%
%  Is true if `cbor_item(Item)` describes
%  a well-formed CBOR serialized into a list of chars.
%  In more pratical terms,
%  if `cbor(Item)` then `phrase(cbor_item(Item), S)` is true
%  and `S` is a list of byte-like elements which
%  is a serialized well-formed CBOR and encodes `Item`.
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
%      for more information `see(cbor_item//1)`.
%
%  The prolog representation used in this library for a CBOR Item
%  maps one-to-one to a serialized form of a certain CBOR Item.
%  A CBOR Value may have multiple serialized forms,
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
%  `see(place_value/2)` for more info.
%
%  Another important functor for the representation
%  is the length indicator.
%  It is either `len(P, N)` or `*` (the atom asterisk).
%  In the first case `len(P, N)`,
%  the length is `N` and it resides at place `P`,
%  also `place_value(P, N)` must be true.
%  In the second case `*`, the length is indefinite.
%  `see(lengthindicator_length/2)` for more info.
%
% # Major 0 -- Unsigned
%
%  It is represented by `unsigned(P, X)`,
%  `X` is the unsigned number itself and
%  `P` is where `X` resides.
%
%  ```prolog
%    cbor(unsigned(P, X)) :- place_value(P, X).
%  ```
%
% # Major 1 -- Negative
%
%  It is represented by `negative(P, X)`,
%  `X` is the negative number itself and
%  `P` is where `X` resides.
%  `V` (in the implementation) is the value
%  in the byte representation.
%
%  ```prolog
%    cbor(negative(P, X)) :-
%      #X #< 0,
%      #X + #V #= -1,
%      place_value(P, V).
%  ```
%
% # Major 2 -- Byte String
%
%  It is represented by `bytes(L, X)`.
%  `L` is a length indicator.
%  If `L = len(P, N)`, then
%  `X` is a list of bytes with length `N`.
%  Otherwise, `L = *` and
%  `X` is a list of Major 2 (byte string) items,
%  and each item has definite length (`L = len(P, N)`).
%  `see(definite_bytes/1)`.
%
%  The following definition is similar to:
%  ```prolog
%    cbor(bytes(len(P, N), X)) :- definite_bytes(bytes(len(P, N), X)).
%    cbor(bytes(*, X)) :- maplist(definite_bytes, X).
%  ```
%  but it puts emphasis on first argument indexing.
%
% # Major 3 -- Text String (UTF-8 encoded)
%
%  It is represented by `text(L, X)`.
%  `L` is a length indicator.
%  If `L = len(P, N)`, then
%  `X` is a list of char and
%  and its underlining represtation uses `N` bytes.
%  Otherwise, `L = *` and
%  `X` is a list of Major 3 (text string) items,
%  and each item has definite length (`L = len(P, N)`).
%  `see(definite_text/1)`.
%
%  The following definition is similar to:
%  ```prolog
%    cbor(text(len(P, N), X)) :- definite_text(text(len(P, N), X)).
%    cbor(text(*, X)) :- maplist(definite_text, X).
%  ```
%  but it puts emphasis on first argument indexing.
%
% # Major 4 -- Array
%
%  It is represented by `array(L, X)`.
%  `L` is a length indicator and
%  `X` is a list of cbor items.
%
%  ```prolog
%    cbor(array(L, X)) :- lengthindicator_length(L, N), length(X, N), maplist(cbor, X).
%  ```
%
% # Major 5 -- Map
%
%  It is represented by `map(L, X)`.
%  `L` is a length indicator and
%  `X` is a list of pairs of cbor items,
%  in another words, each item is of the form `Key-Value` and
%  both `Key` and `Value` are cbor items.
%
%  ```prolog
%    cbor(map(L, X)) :- lengthindicator_length(L, N), length(X, N), maplist(pair_of_cbor, X).
%  ```
%
% # Major 6 -- Tagged Item
%
%  It is represented by `tag(P, T, X)`.
%  `T` is the tag residing in place `P` and
%  `X` is the tagged item, a cbor item.
%
%  ```prolog
%    cbor(tag(P, T, X)) :- place_value(P, T), cbor(X).
%  ```
%
% # Major 7 -- Simple Values
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
%
% # Major 7 -- Floating Point Numbers
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
%  `see(size_int_float/3)`.
%
%  The following definition is similar to:
%  ```prolog
%    cbor(float(x2, X)) :- size_int_float(x2, _, X).
%    cbor(float(x4, X)) :- size_int_float(x4, _, X).
%    cbor(float(x8, X)) :- size_int_float(x8, _, X).
%  ```
%  but it puts emphasis on first argument indexing.
% TODO: use reified version cbor_t
cbor(unsigned(P, X)) :- cbor_unsigned(P, X).
cbor(negative(P, X)) :- cbor_negative(P, X).
cbor(bytes(L, X)) :- cbor_bytes(L, X).
cbor(text(L, X)) :- cbor_text(L, X).
cbor(array(L, X)) :- cbor_array(L, X).
cbor(map(L, X)) :- cbor_map(L, X).
cbor(tag(P, T, X)) :- cbor_tag(P, T), cbor(X).
cbor(simple(P, X)) :- cbor_simple(P, X).
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

cbor_unsigned(P, X) :- place_value(P, X).
cbor_negative(P, X) :- #X #< 0, #X + #V #= -1, place_value(P, V).

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

cbor_array(L, X) :- lengthindicator_length(L, N), length(X, N), maplist(cbor, X).

cbor_map(L, X) :- lengthindicator_length(L, N), length(X, N), maplist(pair_of_cbor, X).
pair_of_cbor(Key-Value) :- cbor(Key), cbor(Value).

cbor_tag(P, T) :- place_value(P, T).

cbor_simple(i , X) :- place_value(i , X).
cbor_simple(x1, X) :- place_value(x1, X), X in 0x20..0xffff.

cbor_float(x2, X) :- size_int_float(x2, _, X).
cbor_float(x4, X) :- size_int_float(x4, _, X).
cbor_float(x8, X) :- size_int_float(x8, _, X).

cbor_pair_(Options, K-V) --> cbor_item_(Options, K), cbor_item_(Options, V).

byte( X) :- X in 0x00..0xff.
short(X) :- X in 0x00..0xffff.
word( X) :- X in 0x00..0xffffffff.
quad( X) :- X in 0x00..0xffffffffffffffff.

byte(char, X) --> [Char], { byte(X), frozen_char_code(Char, X) }.
byte(byte, X) --> [X], { byte(X) }.

frozen_char_code(Char, Code) :-
  ( nonvar(Char) -> char_code(Char, Code)
  ; nonvar(Code) -> char_code(Char, Code)
  ; freeze(Char, char_code(Char, Code)),
    freeze(Code, char_code(Char, Code))
  ).

bytelist([], _) --> [].
bytelist([X | Xs], ListOf) --> byte(ListOf, X), bytelist(Xs, ListOf).

%% phrase(cbor_item(+Item), ?S) is semidet. % doc(cbor_item//1).
%% phrase(cbor_item(?Item), +S) is semidet.
%
%  The predicate `cbor_item//1` is suited for reasoning about CBOR both
%  in its prolog form `Item` and
%  serialized (a list of byte-like elements) form `S`.
%  Encoding (converting prolog into serialized form) and
%  decoding (converting serialized form into prolog form)
%  a CBOR Data Item are examples of
%  what is possible with this predicate and
%  also the main expected use case for it.
%  But it is also possible for checking and completing
%  relations between prolog and serialized forms
%  (more on this in #-Encoding-Features).
%
%  For more information on the prolog form, `see(cbor/1)`.
%  There is a sibling predicate `cbor_item//2`
%  which receives options.
%  To know more about these options, `see(option/3)`.
%
%  The predicate `cbor_item//1` is not suited for stream decoding.
%  It means that
%  it is not possible to use a prefix of
%  a serialized CBOR Data Item to
%  recover a partial information
%  about the prolog form of the same CBOR Data Item.
%
%  The predicate `cbor_item//1` concerns itself with
%  "syntactic valid" CBOR Data Items and
%  does not concern itself with semantically valid CBOR Data Items.
%  The "syntactic valid" CBOR Data Items are
%  a superset of the well-formed CBOR Data Items.
%  The extra items have some substructure wrapped
%  in a nwf functor (short for "not well-formed")
%  in its prolog form.
%  These extra items are included to provide a limited form of
%  partial information retrieval and
%  it may allow to continue the parsing.
%  This feature also allows building workarounds
%  on top of a future extension,
%  for example on using one of the reserved values.
%  This behavior is disabled by default,
%  to enable it use option `on_nwf(wrap)` on `cbor_item//2`.
%  The default behavior (with `on_nwf(fail)`) is failing.
%  (For more on this, `see(nwf_cbor/1)`, `see(option/3)`).
%
% # Encoding Features
%
%  Some CBOR Data Items in prolog form
%  (namely bytes, text, arrays and maps)
%  have redundant information in its representation.
%  The redundant information may not be provided for encoding.
%  For example, while encoding the item
%  `bytes(len(i, N), [0x00, 0x01])`
%  the predicate instantiates `N` to `2`,
%  thus leading to the same result as encoding
%  `bytes(len(i, 2), [0x00, 0x01])`.
%
%  Some CBOR Data Items have many serialized representations
%  depending on which place the information resides in.
%  The predicate `cbor_item//1 supports this.
%  For example, the item unsigned(P, 1) has many possible values for P:
%   * `P = i`
%   * `P = x1`
%   * `P = x2`
%   * `P = x4`
%   * `P = x8`
%  The predicate `cbor_item//1` prefers smaller serialized sizes,
%  ie., the first success is the smallest serialized size.
%  This behavior aligns with
%  the Preferred Serialization (section 4.1 of RFC8949)
%  and allows one to use `once/1` to get
%  the Preferred Serialization.
%
%  For example:
%  ```prolog
%    ?- Bytes = _, phrase(cbor_item(bytes(L, [0x00, 0x01])), Chars), maplist(char_code, Chars, Bytes).
%       Bytes = [66,0,1], L = len(i,2), Chars = "B\x0\\x1\"
%    ;  Bytes = [88,2,0,1], L = len(x1,2), Chars = "X\x2\\x0\\x1\"
%    ;  Bytes = [89,0,2,0,1], L = len(x2,2), Chars = "Y\x0\\x2\\x0\\x1\"
%    ;  Bytes = [90,0,0,0,2,0,1], L = len(x4,2), Chars = "Z\x0\\x0\\x0\\x2\\x0\\x1\"
%    ;  Bytes = [91,0,0,0,0,0,0,0,2,0,1], L = len(x8,2), Chars = "[\x0\\x0\\x0\\x0\\x0\\x0\\x0\\x2\\x0\\x1\"
%    ;  false.
%    ?- Bytes = _, once(phrase(cbor_item(bytes(L, [0x00, 0x01])), Chars)), maplist(char_code, Chars, Bytes).
%       Bytes = [66,0,1], L = len(i,2), Chars = "B\x0\\x1\".
%  ```
%
% # Map Encoding/Decoding Behavior
%
%  Maps are translated as is to/from serialized form.
%  Therefore, there is no sorting of keys,
%  duplicate-keys check or valitity checks.
%
% # Porting Suggestions
%
%  This library was written
%  with [Scryer Prolog](https://scryer.pl) in mind.
%  Therefore, it may not work well with other implementations.
%
% ## `library(clpz)`
%
%  If the target implementation does not support
%  declarative arithmetic,
%  consider splitting `cbor_item//1` into two predicates:
%  one for encoding and another for decoding.
%
%  If the target implementation has support for
%  declarative arithmetic,
%  consider translating the predicate calls.
%  All the used predicates are listed in
%  `:- use_module(library(clpz), [ ... ]).`
%  on the top of the file.
%
cbor_item(X) --> cbor_item(X, []).

%% phrase(cbor_item(+Item, +Options), ?Chars) is semidet. % doc(cbor_item//2).
%% phrase(cbor_item(?Item, +Options), +Chars) is semidet.
%
%  Similar to `cbor_item//1`, but with `Options`.
%  `see(option/3)` for available options.
%
%  `Options` is a list.
%  If repeating options are provided, the first is used.
%
cbor_item(X, OptList) -->
  { parse_options(OptList, Options) },
  cbor_item_(Options, X).

cbor_item_(Options, X) --> cbor_item_(Options, X, nobreak).

cbor_item_(Options, X, B) -->
  cbor_major_value(Major, Value, Options),
  cbor_major_value_x(Major, Value, X, Options, B).

parse_options(OptList, Options) :-
  must_be(list, OptList),
  Options = options(ListOf, BT_2, SIF_3, On_NWF),
  ( member(Opt, OptList), var(Opt) -> instantiation_error(cbor_item//2)
  ; parse_option(listOf(ListOf), OptList),
    parse_option(bytes_text(BT_2), OptList),
    parse_option(float_conversion(SIF_3), OptList),
    parse_option(on_nwf(On_NWF), OptList)
  ).

parse_option(Selector, OptList) :-
  ( member(Selector, OptList) ->
    ( call(Selector) -> true
    ; domain_error(invalid_option, Selector, OptList)
    )
  ; default_option(Selector)
  ).

%% option(+Key, ?Value, ?Options) is semidet. % doc(option/3).
%% option(?Key, ?Value, ?Options) is nondet.
%
%  True if `Options` has a `Value` in place of `Key`.
%  Works similar to `Options.Key` or `Options.Key := Value`
%  in C-like programming languages.
%
%  `Options` are options for `cbor_item//2`.
%  The default values for an option
%  can be found at `default_option/1`.
%
%  `call(Key, Value)` is true
%  iff `O =.. [Key, Value]` and
%  `O` is a valid option for `cbor_item//2`.
%
%  Possible options are:
%
%  * `listOf(ListOf)`:
%    `cbor_item//2` decribes a list of `ListOf`.
%    Possible values: `char` (default), `byte`.
%
%  * `bytes_text(BT_2)`:
%    `BT_2` is a predicate that
%    relates a list of bytes encoding a UTF-8 string `Bytes`
%    to a string `Text`.
%    The predicate is used as `call(BT_2, Bytes, Text)`.
%    The default value is `utf8bytes_chars`
%    which just calls `chars_utf8bytes/2` from `library(charsio)`.
%
%    When using a custom predicate,
%    consider making `BT_2 = Module:Predicate_2` true.
%
%  * `float_convertion(SIF_3)`
%    `SIF_3` is a predicate that
%    relates a `Size` (one of `x2`, `x4`, `x8`),
%    an `Integer` of size `Size` and
%    the `Float` represented by the `Integer`.
%    The predicate is used as `call(SIF_3, Size, Integer, Float)`.
%    `see(size_integer_float/3)` for more information.
%
%    The default value is `size_int_float`.
%    At the moment it is NOT IMPLEMENTED,
%    it behaves the same as `size_int_int`.
%
%    `size_int_int` checks
%    if `Size` is valid and unifies `Integer` with `Float`.
%
%    When using a custom predicate,
%    consider making `SIF_3 = Module:Predicate_3` true.
%
%  * `on_nwf(ON_NWF)`
%    `ON_NWF` decides the behavior when
%    a not well-formed structure is found.
%
%    The default value is `fail`.
%    It makes the predicate fail.
%
%    `wrap` expects the not well-formed substructured `SubItem`
%    inside the functor `nwf`, as in `nwf(SubItem)`.
%
option(listOf          , ListOf, options( ListOf, _BT_2, _SIF_3, _On_NWF)).
option(bytes_text      , BT_2  , options(_ListOf,  BT_2, _SIF_3, _On_NWF)).
option(float_convertion, SIF_3 , options(_ListOf, _BT_2,  SIF_3, _On_NWF)).
option(on_nwf          , On_NWF, options(_ListOf, _BT_2, _SIF_3,  On_NWF)).

%% default_option(+Option) is semidet. % doc(default_option/1).
%% default_option(?Option) is nondet.
%
%  Is true if `Option` is a default option for `cbor_item//2`.
default_option(listOf(char)).
default_option(bytes_text(utf8bytes_chars)).
default_option(float_conversion(size_int_float)).
default_option(on_nwf(fail)).

listOf(char).
listOf(byte).

utf8bytes_chars(Bytes, Text) :- chars_utf8bytes(Text, Bytes).

% bytes_text(utf8bytes_chars).
bytes_text(BT_2) :- callable(BT_2).

% float_conversion(size_int_float).
% float_conversion(size_int_int).
float_conversion(SIF_3) :- callable(SIF_3).

on_nwf(fail).
on_nwf(wrap).

header_major_minor(Header, Major, Minor) :-
  Major in 0..7,
  Minor in 0x00..0x1f,
  #Header #= (#Major << 5) \/ #Minor,
  #Major #= #Header >> 5,
  #Minor #= #Header /\ 0x1f,
true.

cbor_major_minor(Major, Minor, Options) -->
  { option(listOf, ListOf, Options) },
  byte(ListOf, H),
  { header_major_minor(H, Major, Minor) }.

cbor_major_value(Major, Value, Options) -->
  cbor_major_minor(Major, Minor, Options),
  cbor_minor_value(Minor, Value, Options).

cbor_minor_value( 0, val(i,  0), _) --> [].
cbor_minor_value( 1, val(i,  1), _) --> [].
cbor_minor_value( 2, val(i,  2), _) --> [].
cbor_minor_value( 3, val(i,  3), _) --> [].
cbor_minor_value( 4, val(i,  4), _) --> [].
cbor_minor_value( 5, val(i,  5), _) --> [].
cbor_minor_value( 6, val(i,  6), _) --> [].
cbor_minor_value( 7, val(i,  7), _) --> [].
cbor_minor_value( 8, val(i,  8), _) --> [].
cbor_minor_value( 9, val(i,  9), _) --> [].
cbor_minor_value(10, val(i, 10), _) --> [].
cbor_minor_value(11, val(i, 11), _) --> [].
cbor_minor_value(12, val(i, 12), _) --> [].
cbor_minor_value(13, val(i, 13), _) --> [].
cbor_minor_value(14, val(i, 14), _) --> [].
cbor_minor_value(15, val(i, 15), _) --> [].
cbor_minor_value(16, val(i, 16), _) --> [].
cbor_minor_value(17, val(i, 17), _) --> [].
cbor_minor_value(18, val(i, 18), _) --> [].
cbor_minor_value(19, val(i, 19), _) --> [].
cbor_minor_value(20, val(i, 20), _) --> [].
cbor_minor_value(21, val(i, 21), _) --> [].
cbor_minor_value(22, val(i, 22), _) --> [].
cbor_minor_value(23, val(i, 23), _) --> [].
/* NOTE: {true} makes it work for encoding,
 * possibly a bug on scryer side?
 **/
cbor_minor_value(24, val(x1, V), Options) --> {true}, numbytes_number(1, V, Options).
cbor_minor_value(25, val(x2, V), Options) --> {true}, numbytes_number(2, V, Options).
cbor_minor_value(26, val(x4, V), Options) --> {true}, numbytes_number(4, V, Options).
cbor_minor_value(27, val(x8, V), Options) --> {true}, numbytes_number(8, V, Options).
cbor_minor_value(28, reserved_28, _) --> [].
cbor_minor_value(29, reserved_29, _) --> [].
cbor_minor_value(30, reserved_30, _) --> [].
cbor_minor_value(31, indefinite, _) --> [].

cbor_major_value_x(0, Value, X, Options, B) --> cbor_0_value_x(Value, X, Options, B).
cbor_major_value_x(1, Value, X, Options, B) --> cbor_1_value_x(Value, X, Options, B).
cbor_major_value_x(2, Value, X, Options, B) --> cbor_2_value_x(Value, X, Options, B).
cbor_major_value_x(3, Value, X, Options, B) --> cbor_3_value_x(Value, X, Options, B).
cbor_major_value_x(4, Value, X, Options, B) --> cbor_4_value_x(Value, X, Options, B).
cbor_major_value_x(5, Value, X, Options, B) --> cbor_5_value_x(Value, X, Options, B).
cbor_major_value_x(6, Value, X, Options, B) --> cbor_6_value_x(Value, X, Options, B).
cbor_major_value_x(7, Value, X, Options, B) --> cbor_7_value_x(Value, X, Options, B).

cbor_0_value_x(val(P, V), unsigned(P, V), _, _) --> [].
% Not well-formed: 0x00 + 28 = 28
cbor_0_value_x(reserved_28, V, Options, _) --> { options_nwf(Options, 28, V) }.
cbor_0_value_x(reserved_29, V, Options, _) --> { options_nwf(Options, 29, V) }.
cbor_0_value_x(reserved_30, V, Options, _) --> { options_nwf(Options, 30, V) }.
cbor_0_value_x(indefinite , V, Options, _) --> { options_nwf(Options, 31, V) }.

cbor_1_value_x(val(P, V), negative(P, X), _, _) --> { #X #< 0, #X + #V #= -1 }.
% Not well-formed: 0x20 + 28 = 60
cbor_1_value_x(reserved_28, V, Options, _) --> { options_nwf(Options, 60, V) }.
cbor_1_value_x(reserved_29, V, Options, _) --> { options_nwf(Options, 61, V) }.
cbor_1_value_x(reserved_30, V, Options, _) --> { options_nwf(Options, 62, V) }.
cbor_1_value_x(indefinite , V, Options, _) --> { options_nwf(Options, 63, V) }.

cbor_2_value_x(val(P, V), bytes(len(P, V), X), Options, _) --> numberbytes_list(V, X, Options).
% Not well-formed: 0x40 + 28 = 92
cbor_2_value_x(reserved_28, V, Options, _) --> { options_nwf(Options, 92, V) }.
cbor_2_value_x(reserved_29, V, Options, _) --> { options_nwf(Options, 93, V) }.
cbor_2_value_x(reserved_30, V, Options, _) --> { options_nwf(Options, 94, V) }.
cbor_2_value_x(indefinite, bytes(*, X), Options, _) --> indefinite_bytes(X, Options).

cbor_3_value_x(val(P, V), text(len(P, V), X), Options, _) --> numberbytes_text(V, X, Options).
% Not well-formed: 0x60 + 28 = 124
cbor_3_value_x(reserved_28, V, Options, _) --> { options_nwf(Options, 124, V) }.
cbor_3_value_x(reserved_29, V, Options, _) --> { options_nwf(Options, 125, V) }.
cbor_3_value_x(reserved_30, V, Options, _) --> { options_nwf(Options, 126, V) }.
cbor_3_value_x(indefinite, text(*, X), Options, _) --> indefinite_text(X, Options).

cbor_4_value_x(val(P, V), array(len(P, V), X), Options, _) --> numberbytes_array(V, X, Options).
% Not well-formed: 0x80 + 28 = 156
cbor_4_value_x(reserved_28, V, Options, _) --> { options_nwf(Options, 156, V) }.
cbor_4_value_x(reserved_29, V, Options, _) --> { options_nwf(Options, 157, V) }.
cbor_4_value_x(reserved_30, V, Options, _) --> { options_nwf(Options, 158, V) }.
cbor_4_value_x(indefinite, array(*, X), Options, _) --> indefinite_array(X, Options).

cbor_5_value_x(val(P, V), map(len(P, V), X), Options, _) --> numberbytes_map(V, X, Options).
% Not well-formed: 0xa0 + 28 = 188
cbor_5_value_x(reserved_28, V, Options, _) --> { options_nwf(Options, 188, V) }.
cbor_5_value_x(reserved_29, V, Options, _) --> { options_nwf(Options, 189, V) }.
cbor_5_value_x(reserved_30, V, Options, _) --> { options_nwf(Options, 190, V) }.
cbor_5_value_x(indefinite, map(*, X), Options, _) --> indefinite_map(X, Options).

cbor_6_value_x(val(P, V), tag(P, V, X), Options, _) --> cbor_item_(Options, X).
% Not well-formed: 0xc0 + 28 = 220
cbor_6_value_x(reserved_28, V, Options, _) --> { options_nwf(Options, 220, V) }.
cbor_6_value_x(reserved_29, V, Options, _) --> { options_nwf(Options, 221, V) }.
cbor_6_value_x(reserved_30, V, Options, _) --> { options_nwf(Options, 222, V) }.
cbor_6_value_x(indefinite,  V, Options, _) --> { options_nwf(Options, 223, V) }.

cbor_7_value_x(val(P, V), X, Options, _) --> { simple_or_float(P, V, X, Options) }.
% Not well-formed: 0xe0 + 28 = 252
cbor_7_value_x(reserved_28, V, Options, _) --> { options_nwf(Options, 252, V) }.
cbor_7_value_x(reserved_29, V, Options, _) --> { options_nwf(Options, 253, V) }.
cbor_7_value_x(reserved_30, V, Options, _) --> { options_nwf(Options, 254, V) }.
cbor_7_value_x(indefinite,  V, Options, B) --> { break_value(B, V, Options) }.

numbytes_number(1, X, Opts) --> { option(listOf, ListOf, Opts) }, byte(ListOf, X).
numbytes_number(2, X, Opts) --> { N = 1, #X1_ #= #X1 << (8 * N), #X #= #X1_ \/ #X0, #X #= #X1_ xor #X0, #X1 #= #X >> (8 * N) }, numbytes_number(N, X1, Opts), numbytes_number(N, X0, Opts).
numbytes_number(4, X, Opts) --> { N = 2, #X1_ #= #X1 << (8 * N), #X #= #X1_ \/ #X0, #X #= #X1_ xor #X0, #X1 #= #X >> (8 * N) }, numbytes_number(N, X1, Opts), numbytes_number(N, X0, Opts).
numbytes_number(8, X, Opts) --> { N = 4, #X1_ #= #X1 * (2 ^ (8 * N)), #X #= #X1_ \/ #X0, #X #= #X1_ xor #X0, #X1 #= #X >> (8 * N) }, numbytes_number(N, X1, Opts), numbytes_number(N, X0, Opts).
/* NOTE: `X * (2 ^ (8 * 4))` and `X << (8 * 4)` have the same result,
 * unless if `X in 0x80000000..0xffffffff`.
 * This happens, because the result is interpreted as an negative 64bit number.
 */

numberbytes_list(N, L, Options) --> { option(listOf, ListOf, Options), length(L, N) }, bytelist(L, ListOf).
numberbytes_text(N, T, Options) --> numberbytes_list(N, L, Options), { option(bytes_text, BT_2, Options), call(BT_2, L, T) }.
numberbytes_array(N, A, Options) --> { length(A, N) }, foldl(cbor_item_(Options), A).
numberbytes_map(N, M, Options) --> { length(M, N) }, foldl(cbor_pair_(Options), M).

indefinite_bytes(X, Options) --> { option(on_nwf, On_NWF, Options) }, indefinite_help_(X, bytes_uni(On_NWF), indefinite_bytes, Options).
bytes_uni(fail, Out, In) :- bytes_uni_fail(Out, In).
bytes_uni(wrap, Out, In) :- bytes_uni_wrap(Out, In).

bytes_uni_fail(bytes(L, V), bytes(L, V)) :- L = len(_, _).

bytes_uni_wrap(bytes(L, V), bytes(L, V)) :- L = len(_, _).
bytes_uni_wrap(nwf(A), A).

indefinite_text(X, Options) --> { option(on_nwf, On_NWF, Options) }, indefinite_help_(X, text_uni(On_NWF), indefinite_text, Options).
text_uni(fail, Out, In) :- text_uni_fail(Out, In).
text_uni(wrap, Out, In) :- text_uni_wrap(Out, In).

text_uni_fail(text(L, V), text(L, V)) :- L = len(_, _).

text_uni_wrap(text(L, V), text(L, V)) :- L = len(_, _).
text_uni_wrap(nwf(A), A).

indefinite_array(X, Options) --> indefinite_help_(X, =, indefinite_array, Options).

indefinite_map(X, Options) --> indefinite_help_(X, map_uni(Val), indefinite_map_(Val), Options).
indefinite_map_(Val, X, Options) --> cbor_item_(Options, Val), indefinite_map(X, Options).
map_uni(Val, Key-Val, Key).

:- meta_predicate(indefinite_help_(?, 2, 4, ?, ?, ?)).

indefinite_help_([], _, _, Options) --> cbor_item_(Options, break, break).
indefinite_help_([V | X], Out_In, DCG, Options) -->
  { call(cbor:Out_In, V, Item0), if_(dif(Item0, break), Item = Item0, Item = nwf(break)) },
  cbor_item_(Options, Item, nobreak),
  call(cbor:DCG, X, Options).

simple_or_float(i, V, simple(i, V), _) :- V in 0..23.
simple_or_float(x1, V, S, Options) :-
  ( V in 0x00..0x1f, options_nwf(Options, simple(x1, V), S)
  ; V in 0x20..0xff, S = simple(x1, V)
  ).
simple_or_float(x2, V, float(x2, X), Options) :- option(float_convertion, SIF_3, Options), call(SIF_3, x2, V, X).
simple_or_float(x4, V, float(x4, X), Options) :- option(float_convertion, SIF_3, Options), call(SIF_3, x4, V, X).
simple_or_float(x8, V, float(x8, X), Options) :- option(float_convertion, SIF_3, Options), call(SIF_3, x8, V, X).

%% size_int_float(S, I, F). % doc(size_int_float/3).
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
size_int_float(S, I, F) :- size_int_int(S, I, F). % TODO

size_int_int(x2, I, I).
size_int_int(x4, I, I).
size_int_int(x8, I, I).

%% nwf_cbor(NWF). % doc(nwf_cbor/1).
%
% TODO
nwf_cbor(_) :- throw(error(existence_error(procedure,nwf_cbor/1),nwf_cbor/1)).

options_nwf(Options, Item, V) :-
  option(on_nwf, On_NWF, Options),
  options_nwf_(On_NWF, Item, V).

options_nwf_(fail, _, _) :- false.
options_nwf_(wrap, Item, nwf(Item)).

break_value(break  , break, _).
break_value(nobreak, V, Options) :- options_nwf(Options, break, V).
