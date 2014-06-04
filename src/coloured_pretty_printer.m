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
    ---> ansi(ansi_colour, ansi_attrib)
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

:- type ansi_attrib
    ---> none
    ;    normal
    ;    bright_or_bold.

:- instance enum(ansi_colour).

:- type xterm_colour
    ---> xterm_colour(int).

:- instance enum(xterm_colour).

:- func fg(term_colour, doc) = doc.

:- func bg(term_colour, doc) = doc.

:- func underlined(doc) = doc.

:- func overlined(doc) = doc.

:- func colour_on_black(term_colour, doc) = doc.

%----------------------------------------------------------------------------%
%
% Convenience functions for setting the foreground colour (ANSI only)
%

:- func black_fg(doc) = doc.

:- func darkgrey_fg(doc) = doc.

:- func darkred_fg(doc) = doc.

:- func red_fg(doc) = doc.

:- func darkgreen_fg(doc) = doc.

:- func green_fg(doc) = doc.

:- func orange_fg(doc) = doc.

:- func yellow_fg(doc) = doc.

:- func darkblue_fg(doc) = doc.

:- func blue_fg(doc) = doc.

:- func darkmagenta_fg(doc) = doc.

:- func magenta_fg(doc) = doc.

:- func darkcyan_fg(doc) = doc.

:- func cyan_fg(doc) = doc.

:- func grey_fg(doc) = doc.

:- func white_fg(doc) = doc.

%----------------------------------------------------------------------------%
%
% Convenience functions for setting the background colour (ANSI only)
%

:- func black_bg(doc) = doc.

:- func darkred_bg(doc) = doc.

:- func darkgreen_bg(doc) = doc.

:- func orange_bg(doc) = doc.

:- func darkblue_bg(doc) = doc.

:- func darkmagenta_bg(doc) = doc.

:- func darkcyan_bg(doc) = doc.

:- func grey_bg(doc) = doc.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.
:- import_module std_util.
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

%----------------------------------------------------------------------------%

:- instance enum(xterm_colour) where [
    (to_int(xterm_colour(C)) = C),
    (from_int(C) = xterm_colour(C) :- C =< 0xff)
].

%----------------------------------------------------------------------------%

:- instance enum(ansi_attrib) where [
    (to_int(Attrib) = Code   :- ansi_attrib_code(Attrib, Code)),
    (from_int(Code) = Attrib :- ansi_attrib_code(Attrib, Code))
].

:- pred ansi_attrib_code(ansi_attrib, int).
:- mode ansi_attrib_code(in, out) is det.
:- mode ansi_attrib_code(out, in) is semidet.

ansi_attrib_code(none, -1).
ansi_attrib_code(normal, 0).
ansi_attrib_code(bright_or_bold, 1).

%----------------------------------------------------------------------------%
%
% Colourising functions for docs
%

fg(TermColour, Doc) =
    ( TermColour = ansi(ColourCode, Attrib) ->
        ansi_colour_escape(30, ColourCode, Attrib, Doc)
    ; TermColour = xterm(_XTerm) ->
        unexpected($file, $pred, "XTerm colours not supported yet")
    ;
        unexpected($file, $pred, "term colour type not supported")
    ).

bg(TermColour, Doc) =
    ( TermColour = ansi(ColourCode, Attrib) ->
        ansi_colour_escape(40, ColourCode, Attrib, Doc)
    ; TermColour = xterm(_XTerm) ->
        unexpected($file, $pred, "XTerm colours not supported yet")
    ;
        unexpected($file, $pred, "term colour type not supported")
    ).

:- func ansi_colour_escape(int, ansi_colour, ansi_attrib,  doc) = doc.

ansi_colour_escape(Offset, Ansi, Attrib, Doc) =
    docs([( if Attrib = none then
               ansi_sgr(Offset + to_int(Ansi))
            else
               ansi_sgr2(to_int(Attrib), Offset + to_int(Ansi))
          ),
          Doc,
          ansi_sgr(0)]).

underlined(Doc) = ansi_escape_tag(4, 24, Doc).

overlined(Doc) = ansi_escape_tag(53, 55, Doc).

:- func ansi_escape_tag(int, int, doc) = doc.

ansi_escape_tag(Start, End, Doc) =
    docs([ansi_sgr(Start), Doc, ansi_sgr(End)]).

:- func ansi_sgr(int) = doc.

ansi_sgr(Code) = str(format("\u001b[%dm", [i(Code)])).

:- func ansi_sgr2(int, int) = doc.

ansi_sgr2(Attrib, Code) = str(format("\u001b[%d;%dm", [i(Attrib), i(Code)])).

:- func ansi_sgr3(int, int, int) = doc.

ansi_sgr3(Attrib, Fg, Bg) =
    str(format("\u001b[%d;%d;%dm", [i(Attrib), i(Fg), i(Bg)])).

%----------------------------------------------------------------------------%

colour_on_black(ForeColour, Doc) = Docs :-
    ( ForeColour = ansi(Fg0, Attrib0) ->
        ( Fg0 = black, Attrib0 = normal ->
            Fg = white, Attrib = bright_or_bold
        ;
            Fg = Fg0, Attrib = Attrib0
        ),
        FgBgSgr = ansi_sgr3(to_int(Attrib),
                            30 + to_int(Fg),
                            40 + to_int(black)),
        Docs = docs([FgBgSgr, Doc, ansi_sgr(0)])
    ;
        unexpected($file, $module, "Only ANSI colours are supported yet")
    ).

%----------------------------------------------------------------------------%
%
% Implementation of foreground colour conventience functions (ANSI only)
%

black_fg(Doc) = fg(ansi(black, normal), Doc).

darkgrey_fg(Doc) = fg(ansi(black, bright_or_bold), Doc).

darkred_fg(Doc) = fg(ansi(red, normal), Doc).

red_fg(Doc) = fg(ansi(red, bright_or_bold), Doc).

darkgreen_fg(Doc) = fg(ansi(green, normal), Doc).

green_fg(Doc) = fg(ansi(green, bright_or_bold), Doc).

orange_fg(Doc) = fg(ansi(yellow, normal), Doc).

yellow_fg(Doc) = fg(ansi(yellow, bright_or_bold), Doc).

darkblue_fg(Doc) = fg(ansi(blue, normal), Doc).

blue_fg(Doc) = fg(ansi(blue, bright_or_bold), Doc).

darkmagenta_fg(Doc) = fg(ansi(magenta, normal), Doc).

magenta_fg(Doc) = fg(ansi(magenta, bright_or_bold), Doc).

darkcyan_fg(Doc) = fg(ansi(cyan, normal), Doc).

cyan_fg(Doc) = fg(ansi(cyan, bright_or_bold), Doc).

grey_fg(Doc) = fg(ansi(white, normal), Doc).

white_fg(Doc) = fg(ansi(white, bright_or_bold), Doc).

%----------------------------------------------------------------------------%
%
% Implementation of background colour conventience functions (ANSI only)
%

black_bg(Doc) = bg(ansi(black, none), Doc).

darkred_bg(Doc) = bg(ansi(red, none), Doc).

darkgreen_bg(Doc) = bg(ansi(green, none), Doc).

orange_bg(Doc) = bg(ansi(yellow, none), Doc).

darkblue_bg(Doc) = bg(ansi(blue, none), Doc).

darkmagenta_bg(Doc) = bg(ansi(magenta, none), Doc).

darkcyan_bg(Doc) = bg(ansi(cyan, none), Doc).

grey_bg(Doc) = bg(ansi(white, none), Doc).

%----------------------------------------------------------------------------%
:- end_module coloured_pretty_printer.
% -*- Mode: Mercury; column: 80; indent-tabs-mode: nil; tabs-width: 4 -*-
%----------------------------------------------------------------------------%
