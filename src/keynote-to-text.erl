-module('keynote-to-text').

-export([main/1]).

-define(INDEX, "index.apxl").

-spec main([string()]) -> no_return().
main([Filename]) ->
    Files = files(Filename),
    Strings = strings(Filename),
    io:format("FILES~n", []),
    [io:format("~p~n", [File]) || File <- Files],
    io:format("STRINGS~n", []),
    [io:format("~p~n", [String]) || String <- Strings],
    erlang:halt(0).

-spec files(string()) -> [string()].
files(Filename) ->
    {ok, Zip} = zip:zip_open(Filename),
    {ok, Files} = zip:zip_list_dir(Zip),
    [Name || {zip_file, Name, _Info, _, _, _} <- lists:sort(Files)].

-spec strings(string()) -> [string()].
strings(Filename) ->
    {ok, [{?INDEX, Xml}]} = zip:extract(Filename, [{file_list, [?INDEX]}, memory]),
    F = fun({characters, C}, Acc) ->
                [C|Acc];
           (_,Acc) ->
                Acc
        end,
    {ok, Strings, _} = erlsom:parse_sax(Xml, [], F),
    Strings.
