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
:- import_module enum.

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
%
% Colourising for docs
%

:- type term_colour
    ---> ansi(ansi_colour)
    ;    xterm(xterm_colour).

:- type ansi_colour
    ---> black
    ;    red
    ;    green
    ;    yellow
    ;    blue
    ;    magenta
    ;    cyan
    ;    white.

:- instance enum(ansi_colour).

:- type xterm_colour == int.

:- instance enum(xterm_colour).

:- func fg(term_colour, doc) = doc.

:- func bg(term_colour, doc) = doc.

%----------------------------------------------------------------------------%
%
% Convenience functions for setting the foreground colour
%

:- func black_fg(doc) = doc.

:- func red_fg(doc) = doc.

:- func green_fg(doc) = doc.

:- func yellow_fg(doc) = doc.

:- func blue_fg(doc) = doc.

:- func magenta_fg(doc) = doc.

:- func cyan_fg(doc) = doc.

:- func white_fg(doc) = doc.

%----------------------------------------------------------------------------%
%
% Convenience functions for setting the background colour
%

:- func black_bg(doc) = doc.

:- func red_bg(doc) = doc.

:- func green_bg(doc) = doc.

:- func yellow_bg(doc) = doc.

:- func blue_bg(doc) = doc.

:- func magenta_bg(doc) = doc.

:- func cyan_bg(doc) = doc.

:- func white_bg(doc) = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module string.

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
        unexpected($file, $pred, "cannot print") % TODO deconstruct Univ
    ).

%----------------------------------------------------------------------------%
%
% enum typeclass instances for terminal colours
%

:- instance enum(ansi_colour) where [
    (to_int(Ansi) = Code   :- ansi_colour_code(Ansi, Code)),
    (from_int(Code) = Ansi :- ansi_colour_code(Ansi, Code))
].

:- pred ansi_colour_code(ansi_colour, int).
:- mode ansi_colour_code(in, out) is det.
:- mode ansi_colour_code(out, in) is semidet.

ansi_colour_code(black, 0).
ansi_colour_code(red, 1).
ansi_colour_code(green, 2).
ansi_colour_code(yellow, 3).
ansi_colour_code(blue, 4).
ansi_colour_code(magenta, 5).
ansi_colour_code(cyan, 6).
ansi_colour_code(white, 7).

:- instance enum(xterm_colour) where [
    (to_int(C) = C),
    (from_int(C) = C :- C =< 0xff)
].

%----------------------------------------------------------------------------%
%
% Colourising functions for docs
%

fg(TermColour, Doc) =
    ( TermColour = ansi(Ansi) ->
        ansi_colour_escape(30, Ansi, Doc)
    ; TermColour = xterm(_XTerm) ->
        unexpected($file, $pred, "XTerm colours not supported yet")
    ;
        unexpected($file, $pred, "term colour type not supported")
    ).

bg(TermColour, Doc) =
    ( TermColour = ansi(Ansi) ->
        ansi_colour_escape(40, Ansi, Doc)
    ; TermColour = xterm(_XTerm) ->
        unexpected($file, $pred, "XTerm colours not supported yet")
    ;
        unexpected($file, $pred, "term colour type not supported")
    ).

:- func ansi_colour_escape(int, ansi_colour, doc) = doc.

ansi_colour_escape(Offset, Ansi, Doc) =
    docs([str(format("\u001b[%dm", [i(Offset + to_int(Ansi))])),
          Doc,
          str(format("\u001b[%dm", [i(Offset + 9)]))
         ]).

%----------------------------------------------------------------------------%
%
% Implementation of foreground colour conventience functions
%

black_fg(Doc) = fg(ansi(black), Doc).

red_fg(Doc) = fg(ansi(red), Doc).

green_fg(Doc) = fg(ansi(green), Doc).

yellow_fg(Doc) = fg(ansi(yellow), Doc).

blue_fg(Doc) = fg(ansi(blue), Doc).

magenta_fg(Doc) = fg(ansi(magenta), Doc).

cyan_fg(Doc) = fg(ansi(cyan), Doc).

white_fg(Doc) = fg(ansi(white), Doc).

%----------------------------------------------------------------------------%
%
% Implementation of background colour conventience functions
%

black_bg(Doc) = bg(ansi(black), Doc).

red_bg(Doc) = bg(ansi(red), Doc).

green_bg(Doc) = bg(ansi(green), Doc).

yellow_bg(Doc) = bg(ansi(yellow), Doc).

blue_bg(Doc) = bg(ansi(blue), Doc).

magenta_bg(Doc) = bg(ansi(magenta), Doc).

cyan_bg(Doc) = bg(ansi(cyan), Doc).

white_bg(Doc) = bg(ansi(white), Doc).

%----------------------------------------------------------------------------%
:- end_module coloured_pretty_printer.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
