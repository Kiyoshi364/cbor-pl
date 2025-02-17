:- use_module(cbor, [ cbor_item//1 ]).

:- use_module(library(dcgs), [phrase/2]).
:- use_module(library(iso_ext), [setup_call_cleanup/3]).
:- use_module(library(lists), [maplist/2]).
:- use_module(library(pio), [phrase_from_file/2, phrase_to_file/2]).
:- use_module(library(format), [format/2]).

example_cbor(0, unsigned(_, 10)).
example_cbor(1, negative(_, -3)).
example_cbor(2, bytes(_, [0x00, 0x01, 0x02, 0x03, 0x04])).
example_cbor(3, text(_, "hello world!\n")).
example_cbor(4, array(_, [unsigned(_, 2), text(_, "2nd item")])).
example_cbor(5, map(_, [text(_, "key")-text(_, "value"), unsigned(_, 0)-negative(_, -1)])).
example_cbor(6, tag(_, 0, unsigned(_, 10))).
example_cbor(7, simple(_, 20)).
% NOTE: currently floats are represented by their integer representation
% 0x7e00 is a standard 2-byte NAN, and 0x0200 is the canonical payload
example_cbor(8, float(_, special(0, 0x0200))).

default_file("a.out").

main_write :- main_write(0).
main_write(Ex) :- default_file(File), main_write(Ex, File).
main_write(Ex, File) :-
  example_cbor(Ex, Cbor),
  phrase(cbor_item(Cbor), Out),
  setup_call_cleanup(
    open(File, write, Stream),
    ( maplist(put_char(Stream), Out),
      format("Output written to `~s'~n", [File])
    ),
    close(Stream)
  ),
  halt(0).

main_read :- default_file(File), main_read(File).
main_read(File) :-
  setup_call_cleanup(
    open(File, read, Stream),
    ( read_entire_file(Stream, In),
      format("Input read from `~s'~n", [File])
    ),
    close(Stream)
  ),
  phrase(cbor_item(Cbor), In),
  format("~w~n", [Cbor]),
  halt(0).

read_entire_file(Stream, Read) :-
  ( at_end_of_stream(Stream) -> Read = []
  ; get_char(Stream, C),
    Read = [C | Read1],
    read_entire_file(Stream, Read1)
  ).

pure_write :- pure_write(0).
pure_write(Ex) :- default_file(File), pure_write(Ex, File).
pure_write(Ex, File) :-
  example_cbor(Ex, Cbor),
  phrase_to_file(cbor_item(Cbor), File),
  format("Output written to `~s'~n", [File]),
  halt(0).

pure_read :- default_file(File), pure_read(File).
pure_read(File) :-
  phrase_from_file(cbor_item(Cbor), File),
  format("Input read from `~s'~n", [File]),
  format("~w~n", [Cbor]),
  halt(0).
