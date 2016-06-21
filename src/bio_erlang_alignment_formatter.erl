%%% This module generates the strings representing the alignment between two
%%% nucleic acids. It follows the glorious path.
-module(bio_erlang_alignment_formatter).
-export([format/2]).

%% Returns diagn|above|left indicating from where the value came from.
%% 1st param -> 2-ple with previous nucleotides' position.
%% 2nd param -> 2-ple with current nucleotides' position.
from_where({PLine, PColumn}, Current) ->
    case Current of
        {Line, Column} when Line == (PLine+1), Column == (PColumn+1) ->
            diagn;
        {Line, _} when Line == (PLine+1) ->
            above;
        {_, Column} when Column == (PColumn+1) ->
            left
    end.

%% Returns two strings with given characters appended.
%% 1st param -> 2-ple with chars to append.
%% 2nd param -> 2-ple with result strings so far. 
append_results({CharLine, CharColumn}, {Result1, Result2}) ->
    NewResult1 = lists:append([Result1, [CharLine]]),
    NewResult2 = lists:append([Result2, [CharColumn]]),
    {NewResult1, NewResult2}.

%% Returns 2-ple with chars to be appended in result strings.
%% 1st param -> left|above|diagn indicating from where the value came from. 
%% 2nd param -> 2-ple with nucleotides' position.
%% 3rd param -> 2-ple with the sequences.
get_chars(left, {_, Column}, {_, Sequence2}) ->
    {"-", array:get(Column, Sequence2)};
get_chars(above, {Line, _}, {Sequence1, _}) ->
    {array:get(Line, Sequence1), "-"};
get_chars(diagn, {Line, Column}, {Sequence1, Sequence2}) ->
    {array:get(Line, Sequence1), array:get(Column, Sequence2)}.

%% Returns 2-ple with new results' counters.
%% 1st param -> left|above|diagn indicating from where the value came from.
%% 2nd param -> 2-ple with nucleotides' position.
new_counters(left, {LineCounter, ColumnCounter}) ->
    {LineCounter, ColumnCounter+1};
new_counters(above, {LineCounter, ColumnCounter}) ->
    {LineCounter+1, ColumnCounter};
new_counters(diagn, {LineCounter, ColumnCounter}) ->
    {LineCounter+1, ColumnCounter+1}.

%% Returns 2-ple with strings representing the resulted alignment.
%% 1st param -> 2-ple with previous nucleotides' position.
%% 2nd param -> 2-ple with sequences to be aligned.
%% 3rd param -> Alignment's glorious path 
%% 4th param -> 2-ple with result strings so far.
%% 5th param -> 2-ple with counters for current sequences character. 
format_helper(_, _, [], Results, _) ->
    Results;
format_helper(Previous, Sequences, [Current|T], Results, Counters) ->
    From = from_where(Previous, Current),
    Chars = get_chars(From, Counters, Sequences),
    NewResults = append_results(Chars, Results),
    NewCounters = new_counters(From, Counters),
    format_helper(Current, Sequences, T, NewResults, NewCounters).

%% Returns 2-ple with strings representing the resulted alignment.
%% 1st param -> 2-ple with sequences to be aligned.
%% 2nd param -> Score map of alignment.
format(Sequences, GloriousPath) ->
    [H|T] = GloriousPath,
    format_helper(H, Sequences, T, {"", ""}, {1, 1}).