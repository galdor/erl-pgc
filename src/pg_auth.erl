%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(pg_auth).

-export([hash_password_md5/3]).

-spec hash_password_md5(User :: unicode:chardata(),
                        Password :: unicode:chardata(),
                        Salt :: binary()) -> binary().
hash_password_md5(User, Password, Salt) ->
  Key = [User, Password],
  Data = [<<"md5">>, hash_md5_hex([hash_md5_hex([Key]), Salt])],
  iolist_to_binary(Data).

-spec hash_md5_hex(iodata()) -> binary().
hash_md5_hex(Data) ->
  Hash = crypto:hash(md5, Data),
  binary_to_hex_string(Hash).

-spec binary_to_hex_string(binary()) -> binary().
binary_to_hex_string(Bin) ->
  HexData = [io_lib:format("~2.16.0B", [Byte]) || <<Byte:8>> <= Bin],
  string:lowercase(iolist_to_binary(HexData)).
