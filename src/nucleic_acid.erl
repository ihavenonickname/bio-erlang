-module(nucleic_acid).
-compile(needleman_wunsch).
-compile(glorious_path).
-export([align/3, print/1]).

from_where({PLine, PColumn}, Current) ->
    case Current of
        {Line, Column} when Line == (PLine+1), Column == (PColumn+1) ->
            diagn;
        {Line, _} when Line == (PLine+1) ->
            above;
        {_, Column} when Column == (PColumn+1) ->
            left
    end.

append_results({CharLine, CharColumn}, {Result1, Result2}) ->
    NewResult1 = lists:append([Result1, [CharLine]]),
    NewResult2 = lists:append([Result2, [CharColumn]]),
    {NewResult1, NewResult2}.

get_chars(left, {_, Column}, {_, Sequence2}) ->
    {"-", array:get(Column, Sequence2)};
get_chars(above, {Line, _}, {Sequence1, _}) ->
    {array:get(Line, Sequence1), "-"};
get_chars(diagn, {Line, Column}, {Sequence1, Sequence2}) ->
    {array:get(Line, Sequence1), array:get(Column, Sequence2)}.

new_counters(left, {LineCounter, ColumnCounter}) ->
    {LineCounter, ColumnCounter+1};
new_counters(above, {LineCounter, ColumnCounter}) ->
    {LineCounter+1, ColumnCounter};
new_counters(diagn, {LineCounter, ColumnCounter}) ->
    {LineCounter+1, ColumnCounter+1}.

align_helper(_, _, [], Results, _) ->
    Results;
align_helper(Previous, Sequences, [Current|T], Results, Counters) ->
    From = from_where(Previous, Current),
    Chars = get_chars(From, Counters, Sequences),
    NewResults = append_results(Chars, Results),
    NewCounters = new_counters(From, Counters),
    align_helper(Current, Sequences, T, NewResults, NewCounters).

align(String1, String2, Setup) ->
    Sequences = needleman_wunsch:to_sequences(String1, String2),
    ScoreMap = needleman_wunsch:perform(Sequences, Setup),
    [H|T] = glorious_path:find(ScoreMap),
    align_helper(H, Sequences, T, {"", ""}, {1, 1}).

print({String1, String2}) ->
    io:format("~s ~n", [String1]),
    io:format("~s ~n", [String2]).