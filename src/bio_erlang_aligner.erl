%%% Top level API to align two nucleic acids.
%%%
%%% Each nucleic acid (as an DNA or RNA) is represented by a string.
%%% Each char in string represents an nucleotide.
%%% An setup must be infomed, because algorithm used to perform the
%%% alignment requires an set of scores to be used.
%%% 
%%% Three lower-level APIs are used for alignment:
%%% needleman_wunsch, glorious_path and alignment_formatter.
%%% Deeper explanations are presented into these APIs.
-module(bio_erlang_aligner).
-export([align/3]).

%% Returns 2-ple with strings representing the resulted alignment.
%% 1st param -> String to be aligned.
%% 2nd param -> String to be aligned.
%% 3rd param -> Score setup to be used for alignment.
align(String1, String2, Setup) ->
    Sequences = bio_erlang_needleman_wunsch:to_sequences(String1, String2),
    ScoreMap = bio_erlang_needleman_wunsch:perform(Sequences, Setup),
    GloriousPath = bio_erlang_glorious_path:find(ScoreMap),
    bio_erlang_alignment_formatter:format(Sequences, GloriousPath).
