%%% This module performs all operations to create the alignment of nucleic
%%% acid sequences.
-module(needleman_wunsch).
-export([to_sequences/2, perform/2]).

%% Returns match|mismatch indicating if there is a match of nucleotides.
%% 1st param -> A 2-ple with the nucleotides' position.
%% 2nd param -> A 2-ple with the sequences. 
verify_match({Line, Column}, {Sequence1, Sequence2}) ->
    N1 = array:get(Line, Sequence1),
    N2 = array:get(Column, Sequence2),
    case N1 =:= N2 of
        true  -> match;
        false -> mismatch
    end.

%% Returns the numeric value to add in diagonal neighbour.
%% 1st param -> match|mismatch indicating if there was a match.
%% 2nd param -> Numeric score of a match.
%% 3rd param -> Numeric score of a mismatch.
diagonal_addition(match, MatchValue, _) ->
    MatchValue;
diagonal_addition(mismatch, _, MismatchValue) ->
    MismatchValue.

%% Returns the value at given position.
%% 1st param -> A 2-ple with the nucleotides' position.
%% 2nd param -> Map with results so far.
get_value(Point, Scores) ->
    {_, Value} = maps:get(Point, Scores),
    Value.

%% Returns a 3-ple with all neighbours' values at given Point.
%% 1st param -> 2-ple with nucleotides' positions.
%% 2nd param -> Map with results so far.
neighbours({Line, Column}, Scores) ->
    {
        get_value({Line  , Column-1}, Scores), % At left
        get_value({Line-1, Column  }, Scores), % Above
        get_value({Line-1, Column-1}, Scores)  % At upper-left diagonal
    }.

%% Returns the neighbour with greater value.
%% 1st param ->  A list of 2-ples with all neighbours.
max(Neighbours) ->
    Comp = fun ({_, V1}, {_, V2}) -> V1 > V2 end,
    [Max|_] = lists:sort(Comp, Neighbours),
    Max.

%% Returns possible values for comparing two nucleotides.
%% 1st param -> 2-ple with nucleotides' positions.
%% 2nd param -> 2-ple with sequences.
%% 3rd param -> Map with results so far.
%% 4th param -> 3-ple with scores for match, mismatch or gap.
neighbours_values(Point, Sequences, Scores, Setup) ->
    {MatchValue, MismatchValue, Gap} = Setup,
    MatchResult = verify_match(Point, Sequences),
    DiagonalAdd = diagonal_addition(MatchResult, MatchValue, MismatchValue),
    {Line, Column} = Point,
    {Left, Above, Diagn} = neighbours(Point, Scores),
    [
        {{Line-1, Column-1}, Diagn + DiagonalAdd},
        {{Line  , Column-1}, Left  + Gap}, 
        {{Line-1, Column  }, Above + Gap} 
    ].

%% Returns the value for comparing two nucleotides.
%% 1st param -> 2-ple with nucleotides' positions.
%% 2nd param -> 2-ple with sequences.
%% 3rd param -> Map with results so far. 
%% 4th param -> 3-ple with scores for match, mismatch or gap.
generate_value({0, Column}, _, _, {_, _, Gap}) ->
    {none, Column * Gap};
generate_value({Line, 0}, _, _, {_, _, Gap}) ->
    {none, Line * Gap};
generate_value(Point, Sequences, Scores, Setup) ->
    Neighbours = neighbours_values(Point, Sequences, Scores, Setup),
    max(Neighbours).
    
%% Returns a 2-ple with sequences' last Point.
%% 1st param -> 2-ple with sequences.
last_point({Sequence1, Sequence2}) ->
    {
        array:size(Sequence1)-1,
        array:size(Sequence2)-1
    }.

%% Returns a 2-ple with next nucleotides' Point to be compared; or atom nope.
%% 1st param -> 2-ple with nucleotides' positions.
%% 2nd param -> 2-ple with sequences.
next_point({Line, Column}, Sequences) ->
    case last_point(Sequences) of
        {L1, L2} when Line == L1 andalso Column == L2 ->
            nope;
        {_, L2} when L2 == Column ->
            {Line+1, 0};
        _ ->
            {Line, Column+1}
    end.

%% Returns a map with generated values for the given sequences and setup.
%% 1st param -> 2-ple with nucleotides' positions.
%% 2nd param -> 2-ple with sequences.
%% 3rd param -> Map with results so far.
%% 4th param -> 3-ple with scores for match, mismatch or gap.
score_map(Point, Sequences, Scores, Setup) ->
    Value = generate_value(Point, Sequences, Scores, Setup),
    NewScores = maps:put(Point, Value, Scores),

    case next_point(Point, Sequences) of
        nope ->
            NewScores;
        NewPoint ->
            score_map(NewPoint, Sequences, NewScores, Setup)
    end.

%% Returns 
%% 1st param -> 2-ple with sequences.
%% 2nd param -> 3-ple with scores for match, mismatch or gap.
perform(Sequences, Setup) ->
    ScoreMap = score_map({0, 0}, Sequences, maps:new(), Setup),
    {Sequences, Setup, ScoreMap}.

%% Returns a nucleic acid sequence.
%% 1st param -> A string to be converted.
to_sequence(String) ->
    array:from_list([" "|String]).

%% Returns 2-ple with two nucleic acid sequence.
%% 1st param -> A string to be converted.
%% 1st param -> A string to be converted.
to_sequences(String1, String2) ->
    {to_sequence(String1), to_sequence(String2)}.