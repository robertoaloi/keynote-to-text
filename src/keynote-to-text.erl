-module('keynote-to-text').

-export([main/1]).

-define(INDEX, "index.apxl").
-define(SHA_KEY, "keynote").

-record(state, {acc = [], slide_id, slide_content = []}).

-spec main([string()]) -> no_return().
main([Filename]) ->
    Files = files(Filename),
    Slides = slides(Filename),
    print(title("Embedded Files")),
    [print(File) || File <- Files],
    print(title("Slides")),
    [print(Slide) || Slide <- Slides],
    erlang:halt(0).

-spec files(string()) -> [string()].
files(Filename) ->
    {ok, Zip} = zip:zip_open(Filename),
    {ok, Files} = zip:zip_list_dir(Zip),
    filter([Name || {zip_file, Name, _Info, _, _, _} <- lists:sort(Files)]).

-spec slides(string()) -> [string()].
slides(Filename) ->
    {ok, [{?INDEX, Xml}]} = zip:extract(Filename, [{file_list, [?INDEX]}, memory]),
    F = fun({characters, C},
            #state{acc = Acc} = State) ->
                State#state{acc = [C|Acc]};
           ({startElement, _Uri, "slide", _Prefix, Attributes},
            #state{acc = Acc} = State) ->
                SlideId = slide_id(get_attribute("ID", Attributes)),
                SlideTitle = title("Slide " ++ SlideId),
                State#state{acc           = [SlideTitle|Acc],
                            slide_id      = SlideId,
                            slide_content = []};
           ({endElement, _Uri, "slide", _Prefix},
            #state{acc = Acc, slide_id = SlideId} = State) ->
                SlideSha = sha(lists:sort(State#state.slide_content)),
                State#state{acc = ["SHA (Slide " ++ SlideId ++ "): " ++ SlideSha|Acc]};
           ({startElement, _Uri, "master-slide", _Prefix, Attributes},
            #state{acc = Acc} = State) ->
                SlideId = slide_id(get_attribute("ID", Attributes)),
                SlideName = get_attribute("name", Attributes),
                SlideTitle = title("Master Slide " ++ SlideId ++ " (" ++ SlideName ++ ")"),
                State#state{acc           = [SlideTitle|Acc],
                            slide_id      = SlideId,
                            slide_content = []};
           ({endElement, _Uri, "master-slide", _Prefix},
            #state{acc = Acc, slide_id = SlideId} = State) ->
                SlideSha = sha(lists:sort(State#state.slide_content)),
                State#state{acc = ["SHA (Slide " ++ SlideId ++ "): " ++ SlideSha|Acc]};
           ({startElement, _Uri, "notes", _Prefix, _Attributes},
            #state{acc = Acc} = State) ->
                State#state{acc = [subtitle("Notes")|Acc]};
           ({startElement, Uri, Tag, Prefix, _Attributes},
            #state{slide_content = SlideContent} = State) ->
                State#state{slide_content = [{Uri, Tag, Prefix}|SlideContent]};
           (_Other, State) ->
                State
        end,
    {ok, State, _} = erlsom:parse_sax(Xml, #state{}, F),
    lists:reverse(State#state.acc).

filter(Filenames) ->
    filter(Filenames, []).

filter([], Acc) ->
    Acc;
filter(["QuickLook/" ++ _|T], Acc) ->
    filter(T, Acc);
filter(["thumbs/" ++ _|T], Acc) ->
    filter(T, Acc);
filter([".iWTrash/" ++ _|T], Acc) ->
    filter(T, Acc);
filter([H|T], Acc) ->
    filter(T, [H|Acc]).

get_attribute(Tag, Attributes) ->
    {value, Attribute} = lists:keysearch(Tag, 2, Attributes),
    {attribute, Tag, _, _, Value} = Attribute,
    Value.

title(String) ->
    "==> " ++ String.

subtitle(String) ->
    "---" ++ String.

sha(Term) ->
    <<Mac:160/integer>> = crypto:sha_mac(?SHA_KEY, term_to_binary(Term)),
    lists:flatten(io_lib:format("~40.16.0b", [Mac])).

slide_id("BGSlide-" ++ X) ->
    integer_to_list(list_to_integer(X) + 1);
slide_id("BGMasterSlide-" ++ X) ->
    "Master " ++ integer_to_list(list_to_integer(X) + 1).

print(String) ->
    io:format("~p~n", [String]).
