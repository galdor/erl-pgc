%% Copyright (c) 2020-2021 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(pgc_codec_enum).

-behaviour(pgc_codec).

-export([encode/4, decode/4]).

-spec encode(atom() | binary(), pgc_types:type(),
             pgc_types:type_set(), list()) -> binary().
encode(Value, _, _, [_EnumName]) when is_atom(Value)->
  atom_to_binary(Value);
encode(Value, _, _, [_EnumName]) when is_binary(Value) ->
  Value.

-spec decode(binary(), pgc_types:type(), pgc_types:type_set(), list()) ->
        unicode:chardata().
decode(Bin, _, _, [_EnumName]) ->
  binary_to_atom(Bin).
