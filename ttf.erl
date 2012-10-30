#!/opt/local/bin/escript

main(_) ->
    Filename = 'cgrtr.ttf',
    {ok, Bin} = file:read_file(Filename),
    {[_, NumTables | _], Bin2} = get_offset_subtable(Bin),
    io:format("num tables: [~p]~n", [NumTables]),
    get_table_info(Bin2, NumTables).

get_table_info(Bin, 0) ->
    Bin;
get_table_info(<<Tag:4/signed-unit:8-binary, CheckSum:32, Offset:32, Length:32, Bin2/binary>>, Num) ->
    io:format("get_table_info~n"),
    io:format("tag: [~p]~n", [binary_to_list(Tag)]),
    io:format("check sum: [~p]~n", [CheckSum]),
    io:format("offset: [~p]~n", [Offset]),
    io:format("length: [~p]~n", [Length]),
    get_table_info(Bin2, Num - 1).

get_offset_subtable(Bin) ->
    <<ScalarType:32, NumTables:16, SearchRange:16, EntrySelector:16, RangeShift:16, Bin2/binary>> = Bin,
    {[ScalarType, NumTables, SearchRange, EntrySelector, RangeShift], Bin2}.

