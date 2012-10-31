-module(ttf_parse).
-compile(export_all).
-include("ttf_offset_table.hrl").
-include("ttf_table.hrl").
-include("ttf_table_glyf.hrl").
-include("ttf_table_simple_glyph_description.hrl").
-include("ttf_table_simple_glyph_description_flag.hrl").

-define(FILENAME, "font.d/subset.ttf").

% Reference: http://www.microsoft.com/typography/otspec/otff.htm
% all have big endien
% BYTE: 8-bit unsigned integer
% CHAR: 8-bit signed integer
% USHORT: 16-bit unsigned integer
% SHORT: 16-bit signed integer

% ULONG: 32-bit unsigned integer

% Fixed: 32-bit, signed fixed-point number (16.16)

read() ->
    read(?FILENAME).

read(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    Bin.

parse() ->
    parse_font(read()).

parse(Filename) ->
    parse_font(read(Filename)).

get_offset_table(Bin) ->
    <<SfntVersionP1:16/signed-integer,
      SfntVersionP2:16/signed-integer,
      NumTables:16/unsigned-integer,
      SearchRange:16/unsigned-integer,
      EntrySelector:16/unsigned-integer,
      RangeShift:16/unsigned-integer,
      Bin2/binary>> = Bin,
    SfntVersion = SfntVersionP1 + SfntVersionP2 / 10,
    case NumTables * 16 - SearchRange of
        RangeShift -> ok
    end,
    io:format("get_offset_table~n"),
    io:format("\tsfnt_version:\t~p~n", [SfntVersion]),
    io:format("\tnum_tables:\t~p~n", [NumTables]),
    io:format("\tsearch_range:\t~p~n", [SearchRange]),
    io:format("\tentry_selector:\t~p~n", [EntrySelector]),
    io:format("\trange_shift:\t~p~n", [RangeShift]),
    {#ttf_offset_table{sfnt_version = SfntVersion,
                       num_tables = NumTables,
                       search_range = SearchRange,
                       entry_selector = EntrySelector,
                       range_shift = RangeShift}, Bin2}.

get_tables(Font, OffsetTable) ->
    get_tables_acc(Font, OffsetTable#ttf_offset_table.num_tables, []).

get_tables_acc(_Font, 0, Tables) ->
    Tables;
get_tables_acc(Font, Num, List) when Num > 0 ->
    <<Tag1,
      Tag2,
      Tag3,
      Tag4,
      CheckSum:32/unsigned-integer,
      Offset:32/unsigned-integer,
      Length:32/unsigned-integer,
      Font2/binary>> = Font,
    Tag = [Tag1, Tag2, Tag3, Tag4],
    Table = #ttf_table{tag = Tag,
                       check_sum = CheckSum,
                       offset = Offset,
                       length = Length},
    %% io:format("get_tables_acc~n"),
    %% io:format("\ttag:\t~p~n", [Tag]),
    %% io:format("\tcheck_sum:\t~p~n", [CheckSum]),
    %% io:format("\toffset:\t~p~n", [Offset]),
    %% io:format("\tlength:\t~p~n", [Length]),
    get_tables_acc(Font2, Num - 1, [Table | List]).

%%% Required Tables
%%%% cpam Table
%%%% .notdef: missing glyph

parse_table_cmap(_Bin, _Table) ->
    ok.

parse_table_loca(_Font, _Table) ->
    ok.

%%% Tables Related to TrueType Outlines
%%%% numberOfContour: SHORT
%%%% xMin: SHORT
%%%% yMin: SHORT
%%%% xMax: SHORT
%%%% yMax: SHORT

%%%% Simeple Glyph Description
%%%%% endPtsOfContours[n]: USHORT

get_ushorts_from_binary(Bin, Count) ->
    get_ushorts_from_binary_acc(Bin, Count, []).

get_ushorts_from_binary_acc(Bin, 0, List) ->
    {List, Bin};
get_ushorts_from_binary_acc(Bin, Count, List) when Count > 0 ->
    <<Int:16/unsigned-integer, Bin2/binary>> = Bin,
    get_ushorts_from_binary_acc(Bin2, Count - 1, [Int | List]).

get_bytes_from_binary(Bin, Count) ->
    get_bytes_from_binary_acc(Bin, Count, []).

get_bytes_from_binary_acc(Bin, 0, List) ->
    {List, Bin};
get_bytes_from_binary_acc(Bin, Count, List) when Count > 0 ->
    <<Int:8/unsigned-integer, Bin2/binary>> = Bin,
    get_bytes_from_binary_acc(Bin2, Count - 1, [Int | List]).

%% parse_table_simple_glyph_description_acc(Font, [], XCoodinates, YCoodinates) ->
%%     {Font, XCoodinates, YCoodinates};
%% parse_table_simple_glyph_description_acc(Font, [Flag | Flags], XCoodinates, YCoodinates) ->

read_repeat_flags(Font, Flag) when Flag#ttf_table_simple_glyph_description_flag.repeat > 0 ->
    <<RepeatCount, Font2/binary>> = Font,
    Flags = lists:duplicate(RepeatCount, Flag),
    {[Flags | Flag], Font2};
read_repeat_flags(Font, Flag) ->
    {[Flag], Font}.

read_flags(Font, Count) ->
    read_flags_acc(Font, Count, []).

read_flags_acc(Font, 0, List) ->
    {List, Font};
read_flags_acc(Font, Count, List) when Count > 0 ->
    io:format("read_flags_acc~n"),
    <<Flag:8/bitstring, Font2/binary>> = Font,
    io:format("\tflag:\t~p~n", [Flag]),
    OnCurve      = binary:decode_unsigned(Flag) band 2#00000001 > 0,
    XShortVector = binary:decode_unsigned(Flag) band 2#00000010 > 0,
    YShortVector = binary:decode_unsigned(Flag) band 2#00000100 > 0,
    Repeat       = binary:decode_unsigned(Flag) band 2#00001000 > 0,
    ThisXIsSame  = binary:decode_unsigned(Flag) band 2#00010000 > 0,
    ThisYIsSame  = binary:decode_unsigned(Flag) band 2#00100000 > 0,
    0            = binary:decode_unsigned(Flag) band 2#01000000,
    0            = binary:decode_unsigned(Flag) band 2#10000000,
    Flag2 = #ttf_table_simple_glyph_description_flag{on_curve = OnCurve,
                                                     x_short_vector = XShortVector,
                                                     y_short_vector = YShortVector,
                                                     repeat = Repeat,
                                                     this_x_is_same = ThisXIsSame,
                                                     this_y_is_same = ThisYIsSame},
    io:format("\ton_curve:\t~p~n", [OnCurve]),
    io:format("\tx_short_vector:\t~p~n", [XShortVector]),
    io:format("\ty_short_vector:\t~p~n", [YShortVector]),
    io:format("\trepeat:\t~p~n", [Repeat]),
    io:format("\tthis_x_is_same:\t~p~n", [ThisXIsSame]),
    io:format("\tthis_y_is_same:\t~p~n", [ThisYIsSame]),
    {Flags, Font2} = read_repeat_flags(Font, Flag2),
    read_flags_acc(Font2, Count - 1, [Flags | List]).

parse_table_simple_glyph_description(Font, TableGlyf) ->
    io:format("fun parse_table_simple_glyph_description~n"),
    {EndPtsOfContours, Font2} = get_ushorts_from_binary(Font, TableGlyf#ttf_table_glyf.number_of_contours),
    io:format("\tEndPtsOfContours:\t~p~n", [EndPtsOfContours]),
    <<InstructionLength:16/unsigned-integer, Font3/binary>> = Font2,
    io:format("\tInstructionLength:\t~p~n", [InstructionLength]),
    {Instructions, Font4} = get_bytes_from_binary(Font3, InstructionLength),
    io:format("\tInstructions:\t~p~n", [Instructions]),
    {List, Font5} = get_bytes_from_binary(Font4, InstructionLength),
    %%% TODO: enable ThisXIsSame, and ThisYIsSame, these does not work now.
    {Flags, Font6} = read_flags(Font5, TableGlyf#ttf_table_glyf.number_of_contours),

    Flags = lists:map(fun (X) -> <<OnCurve:1,
                                   XShortVector:1,
                                   YShortVector:1,
                                   Repeat:1,
                                   ThisXIsSame:1,
                                   ThisYIsSame:1,
                                   0:1,
                                   0:1>> = X,
                                 #ttf_table_simple_glyph_description_flag{on_curve = OnCurve,
                                                                          x_short_vector = XShortVector,
                                                                          y_short_vector = YShortVector,
                                                                          repeat = Repeat,
                                                                          this_x_is_same = ThisXIsSame,
                                                                          this_y_is_same = ThisYIsSame}
                      end, List),
    io:format("\tFlags:\t~p~n", [Flags]),
%    {Font6, XCoodinates, YCoodinates} = parse_table_simple_glyph_description_acc(Font5, Flags, [], []),
    ok.

parse_table_composite_glyph_description(_Font2, _TableGlyf) ->
    true = false.

parse_table_glyf_acc(<<_:0/binary>>, Glyphs) ->
    Glyphs;
parse_table_glyf_acc(Font, Glyphs) ->
    io:format("\tparse_table_glyf~n"),
    <<NumberOfContours:16/signed-integer,
      XMin:16/signed-integer,
      YMin:16/signed-integer,
      XMax:16/signed-integer,
      YMax:16/signed-integer,
      Font2/binary>> = Font,
    TableGlyf = #ttf_table_glyf{number_of_contours = NumberOfContours,
                                x_min = XMin,
                                y_min = YMin,
                                x_max = XMax,
                                y_max = YMax},
    io:format("\t\tnumber of contour:\t~p~n", [TableGlyf#ttf_table_glyf.number_of_contours]),
    io:format("\t\tx min:\t~p~n", [TableGlyf#ttf_table_glyf.x_min]),
    io:format("\t\ty min:\t~p~n", [TableGlyf#ttf_table_glyf.y_min]),
    io:format("\t\tx max:\t~p~n", [TableGlyf#ttf_table_glyf.x_max]),
    io:format("\t\ty max:\t~p~n", [TableGlyf#ttf_table_glyf.y_max]),
    case TableGlyf#ttf_table_glyf.number_of_contours >= 0 of
        true  -> {Font3, Glyph} = parse_table_simple_glyph_description(Font2, TableGlyf);
        false -> {Font3, Glyph} = parse_table_composite_glyph_description(Font2, TableGlyf)
    end,
    parse_table_glyf_acc(Font3, [Glyph | Glyphs]).

parse_table_glyf(Font, _Table) ->
    parse_table_glyf_acc(Font, []).

%%% Tables Related to PostScript Outlines

%%% Tables Related to Bitmap Glyphs

%%% Advanced Typographic Tables

%%% Other OpenType Tables

%%% Not Categorized Yet

parse_table_prep(_Font, _Table) ->
    ok.

parse_table_post(_Font, _Table) ->
    ok.

parse_table_name(_Font, _Table) ->
    ok.

parse_table_maxp(_Font, _Table) ->
    ok.

parse_table_hmtx(_Font, _Table) ->
    ok.

parse_table_hhea(_Font, _Table) ->
    ok.

parse_table_head(_Font, _Table) ->
    ok.

parse_table_gasp(_Font, _Table) ->
    ok.

parse_table_fpgm(_Font, _Table) ->
    ok.

parse_table_cvt_(_Font, _Table) ->
    ok.

parse_table_os_2(_Font, _Table) ->
    ok.

parse_table_gsub(_Font, _Table) ->
    ok.

parse_table_vmtx(_Font, _Table) ->
    ok.

parse_table_vhea(_Font, _Table) ->
    ok.

parse_table_gpos(_Font, _Table) ->
    ok.

parse_table_gdef(_Font, _Table) ->
    ok.

parse_table_fftm(_Font, _Table) ->
    ok.

get_method_name(Tag) ->
%    io:format("pre conv:\t~p~n", [Tag]),
    String = "parse_table_" ++ re:replace(string:to_lower(Tag), " |/", "_", [{return, list}]),
%    io:format("aft conv:\t~p~n", [String]),
    list_to_atom(String).

parse_table(Font, Table) ->
    io:format("parse_table~n"),
    io:format("\ttag:\t~p~n", [Table#ttf_table.tag]),
    io:format("\toffset:\t~p~n", [Table#ttf_table.offset]),
    io:format("\tlength:\t~p~n", [Table#ttf_table.length]),
    Bin = binary_part(Font, Table#ttf_table.offset, Table#ttf_table.length),
    apply(?MODULE, get_method_name(Table#ttf_table.tag), [Bin, Table]).

parse_tables(_Font, []) ->
    ok;
parse_tables(Font, [Table | Tables]) ->
    parse_table(Font, Table),
    parse_tables(Font, Tables).

parse_font(Font) ->
    {OffsetTable, Font2} = get_offset_table(Font),
    Tables = get_tables(Font2, OffsetTable),
    io:format("tables: ~p~n", [Tables]),
    parse_tables(Font, Tables).
