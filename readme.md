# cbor.pl

A simple library for quickly lifting one out of raw bytes
without losing any possible representation for CBOR Values.
Possibly a good foundation for building
a higher-level CBOR encoder/decoder.

This library was made
with [Scryer Prolog](https://github.com/mthom/scryer-prolog) in mind.

Features:

* Single predicate for encoding and decoding CBOR `cbor_item//1`
* There is `cbor_item//2` variant with some options (search for `doc(option/3)`)
* For encoding a CBOR Item, some values may be variables, for instance both results in the same encoding:
  ```prolog
  ?- phrase(cbor_item(bytes(len(i, 2), [0x00, 0x01]), [listOf(byte)]), S).
     S = [66,0,1]
  ;  false.
  ?- phrase(cbor_item(bytes(len(i, N), [0x00, 0x01]), [listOf(byte)]), S).
     N = 2, S = [66,0,1]
  ;  false.
  ```

## Dependencies

The dependencies can be found near the top of the file
in lines with `:- use_module(...).`.

* Definite Clause Grammars (DCGs)
* [Constraint Logic Programming over Integers (CLPZ)](https://github.com/triska/clpz) (in Scryer Prolog's standard library)
* `freeze/2` from [`library(freeze)`](https://github.com/mthom/scryer-prolog/blob/master/src/lib/freeze.pl) (maybe a builtin predicate)
* `member/2`, `foldl/4`, `maplist/2` (docs), `length/2` from [`library(lists)`](https://github.com/mthom/scryer-prolog/blob/master/src/lib/lists.pl) (maybe builtin predicates)
* `if_/3`, `dif/3` from [`library(reif)`](https://github.com/mthom/scryer-prolog/blob/master/src/lib/error.pl) (depends on `dif/2`, maybe a builtin predicate)

### Optional Dependencies

These dependencies are "optional",
because they are easy to patch out or port
into another implementation of the library.

* `chars_utf8bytes/2` from [`library(charsio)`](https://github.com/mthom/scryer-prolog/blob/master/src/lib/charsio.pl) (easy to patch out or port)
* `must_be/2`, `instantiation_error/1`, `domain_error/3` from [`library(error)`](https://github.com/mthom/scryer-prolog/blob/master/src/lib/charsio.pl)

# Mirrors

* [github.com/Kiyoshi364/cbor-pl](https://github.com/Kiyoshi364/cbor-pl)
* [gitlab.com/Hashi364/cbor-pl](https://gitlab.com/Hashi364/cbor-pl)

# How to use

1. Download [cbor.pl](cbor.pl)

2. Import the library:
```prolog
:- use_module(cbor).
```

3. Try using `cbor_item//1` or `cbor_item//2`:
```prolog
?- phrase(cbor_item(unsigned(_, 3)), S).
   S = "\x3\"
;  S = "\x18\\x3\"
;  S = "\x19\\x0\\x3\"
;  S = "\x1a\\x0\\x0\\x0\\x3\"
;  S = "\x1b\\x0\\x0\\x0\\x0\\x0\\x0\\x0\\x3\"
;  false.
?- phrase(cbor_item(unsigned(_, 3), [listOf(byte)]), S).
   S = [3]
;  S = [24,3]
;  S = [25,0,3]
;  S = [26,0,0,0,3]
;  S = [27,0,0,0,0,0,0,0,3]
;  false.
```

## How to learn the library

* Option 1: read documentation (comments inside [cbor.pl](cbor.pl)).
  Consider searching for `doc(Pred/N).`, where `Pred` is your desired predicate with arity `N`.
  Consider reading documentation for exported predicates under `% Documentation predicates`.

* Option 2: read code (consider [cbor.pl](cbor.pl), [example.pl](example.pl), [tests.pl](tests.pl)).
  Consider using `grep -v '^%' cbor.pl`.
