%----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%----------------------------------------------------------------------------%
% File: coloured_pretty_printer.m
% Copyright © 2014 Sebastian Godelet
% Main author: Sebastian Godelet <sebastian.godelet+github@gmail.com>
% Created on: Mon May 12 16:03:06 CEST 2014
%
% Pretty print routines for ISO/IEC 6429 colourised output
% See: https://en.wikipedia.org/wiki/ANSI_escape_code
%
%----------------------------------------------------------------------------%

:- module coloured_pretty_printer.

:- interface.

%----------------------------------------------------------------------------%

:- import_module io.
:- import_module list.
:- import_module pretty_printer.
:- import_module univ.

%----------------------------------------------------------------------------%
%
% Helper types for adding formatting descriptors
%

    % update_formatters/3 will use the get_default_formatter_map,
    % add the fmts list to the formatter_map sequentially,
    % and call set_default_formatter_map with the new map.
:- pred update_formatters(fmts::in(fmts), io::di, io::uo) is det.

:- type fmts == list(fmt).

:- inst fmts == list_skel(fmt).

:- type fmt
    ---> fmt(
            fmt_module    :: string,    % FQN Module name
            fmt_name      :: string,    % Type name
            fmt_arity     :: int,       % Type arity
            fmt_formatter :: formatter  % pretty_printer function
         ).

:- inst fmt
    ---> fmt(ground, ground, ground, formatter_func).

:- inst formatter_func == (func(in, in) = out is det).

:- func fmt_any((func(T) = doc)) `with_type` formatter.

:- pred set_formatter_sv(fmt::in, formatter_map::in, formatter_map::out)
    is det.

%----------------------------------------------------------------------------%
%
% Universal helper functions for docs
%

:- func empty_str = doc.

:- func space = doc.

:- func map_to_univ(list(T)) = list(univ).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%----------------------------------------------------------------------------%

map_to_univ(List) = map((func(E) = univ(E)), List).

empty_str = str("").

space = str(" ").

%----------------------------------------------------------------------------%
%
% Formatter update API
%

update_formatters(Fmts, !IO) :-
    get_default_formatter_map(FMap0, !IO),
    foldl(set_formatter_sv, Fmts, FMap0, FMap),
    set_default_formatter_map(FMap, !IO).

set_formatter_sv(Fmt, !FMap) :-
    !:FMap = set_formatter(Fmt^fmt_module, Fmt^fmt_name,
                         Fmt^fmt_arity, Fmt^fmt_formatter, !.FMap).

fmt_any(Fun, Univ, _Args) =
    ( Univ = univ(X) ->
        Fun(X)
    ;
        unexpected($file, $module, "cannot print") % TODO deconstruct Univ
    ).

%----------------------------------------------------------------------------%
:- end_module coloured_pretty_printer.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
