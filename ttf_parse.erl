-module(ttf_parse).
-compile(export_all).
-include("ttf_offset_table.hrl").
-include("ttf_table.hrl").
-include("ttf_table_glyf.hrl").
-include("ttf_table_simple_glyf_description.hrl").

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
    {Bin2, #ttf_offset_table{sfnt_version = SfntVersion,
                             num_tables = NumTables,
                             search_range = SearchRange,
                             entry_selector = EntrySelector,
                             range_shift = RangeShift}}.

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

%% parse_table_simple_glyf_description(Font, TableGlyf) when TableGlyf#ttf_table_glyf.number_of_contours > 0 ->
%%     <<EndPtsOfContours:16/unsigned-integer>> = binary_part(Font, 0, 2),
%%     case EndPtsOfContours of
%%     <<EndPtsOfContours:16/unsigned-integer, _/binary>> = 
%%         <<EndPtsOfContours:16/unsigned-integer,
%%       InstructionLength:16/unsigned-integer,
%%       Instructions:8/unsigned-integer,
%%       Flags:8/unsigned-integer,
      
%%     Font,
%%     ok.

parse_table_glyf(Font, _Table) ->
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
                                y_max = XMax},
    io:format("\t\tnumber of contour:\t~p~n", [TableGlyf#ttf_table_glyf.number_of_contours]),
    io:format("\t\tx min:\t~p~n", [XMin]),
    io:format("\t\ty min:\t~p~n", [YMin]),
    io:format("\t\tx max:\t~p~n", [XMax]),
    io:format("\t\ty max:\t~p~n", [YMax]),
%%     parse_table_simple_glyf_description(Font2, TableGlyf),
    ok.


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
%    io:format("pre conv: ~p~n", [Tag]),
    String = "parse_table_" ++ re:replace(string:to_lower(Tag), " |/", "_", [{return, list}]),
%    io:format("aft conv: ~p~n", [String]),
    list_to_atom(String).

parse_table(Font, Table) ->
    io:format("tag: ~p~n", [Table#ttf_table.tag]),
    io:format("\toffset: ~p~n", [Table#ttf_table.offset]),
    io:format("\tlength: ~p~n", [Table#ttf_table.length]),
    Bin = binary_part(Font, Table#ttf_table.offset, Table#ttf_table.length),
    apply(?MODULE, get_method_name(Table#ttf_table.tag), [Bin, Table]).

parse_tables(_Font, []) ->
    ok;
parse_tables(Font, [Table | Tables]) ->
    parse_table(Font, Table),
    parse_tables(Font, Tables).

parse_font(Font) ->
    {Font2, OffsetTable} = get_offset_table(Font),
    Tables = get_tables(Font2, OffsetTable),
    parse_tables(Font, Tables).
