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

-module(pgc_client).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/0, start_link/1, start_link/2, stop/1,
         simple_exec/2, exec/2, exec/3, exec/4, query/2, query/3, query/4]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([name/0, ref/0, options/0]).

-type name() :: et_gen_server:name().
-type ref() :: et_gen_server:ref().

-type options() :: #{host => unicode:chardata(),
                     port => inet:port_number(),
                     tcp_options => [gen_tcp:connect_option()],
                     tls => boolean(),
                     tls_options => [ssl:tls_client_option()],
                     user => unicode:chardata(),
                     password => unicode:chardata(),
                     database => unicode:chardata(),
                     application_name => unicode:chardata(),
                     types => pgc_types:type_set(),
                     log_messages => boolean(),
                     log_backend_notices => boolean()}.

-type state() :: #{options := options(),
                   socket => inet:socket(),
                   ssl_socket => ssl:sslsocket()}.

-spec validate_options(options()) -> ok.
validate_options(Options) ->
  maps:is_key(user, Options) orelse error(missing_user),
  maps:is_key(database, Options) orelse error(missing_database),
  ok.

-spec start_link() -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link() ->
  start_link(#{}).

-spec start_link(name() | options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Options) when is_map(Options) ->
  gen_server:start_link(?MODULE, [Options], []);
start_link(Name) ->
  start_link(Name, #{}).

-spec start_link(name(), options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Name, Options) ->
  gen_server:start_link(Name, ?MODULE, [Options], []).

-spec stop(ref()) -> ok.
stop(Ref) ->
  gen_server:stop(Ref).

-spec simple_exec(ref(), Query :: unicode:chardata()) -> pgc:exec_result().
simple_exec(Ref, Query)  ->
  Msg = {simple_query, Query},
  gen_server:call(Ref, Msg, infinity).

-spec exec(ref(), Query :: unicode:chardata()) -> pgc:exec_result().
exec(Ref, Query)  ->
  exec(Ref, Query, [], #{}).

-spec exec(ref(), Query :: unicode:chardata(), Params :: [term()]) ->
        pgc:exec_result().
exec(Ref, Query, Params) ->
  exec(Ref, Query, Params, #{}).

-spec exec(ref(), Query :: unicode:chardata(), Params :: [term()],
           pgc:query_options()) -> pgc:exec_result().
exec(Ref, Query, Params, Options) ->
  Msg = {extended_query, Query, Params, Options},
  case gen_server:call(Ref, Msg, infinity) of
    {ok, _Columns, _Rows, NbAffectedRows} ->
      {ok, NbAffectedRows};
    {error, Reason} ->
      {error, Reason}
  end.

-spec query(ref(), Query :: unicode:chardata()) -> pgc:query_result().
query(Ref, Query) ->
  query(Ref, Query, [], #{}).

-spec query(ref(), Query :: unicode:chardata(), Params :: [term()]) ->
        pgc:query_result().
query(Ref, Query, Params) ->
  query(Ref, Query, Params, #{}).

-spec query(ref(), Query :: unicode:chardata(), Params :: [term()],
            pgc:query_options()) -> pgc:query_result().
query(Ref, Query, Params, Options) ->
  gen_server:call(Ref, {extended_query, Query, Params, Options}, infinity).

init([Options]) ->
  logger:update_process_metadata(#{domain => [pgc, client]}),
  validate_options(Options),
  State = #{options => Options},
  Steps = [fun connect/1,
           fun maybe_tls_connect/1,
           fun begin_startup/1,
           fun authenticate/1,
           fun finish_startup/1],
  Res = lists:foldl(fun (Step, {ok, State1}) ->
                        Step(State1);
                        (_, {error, Reason}) ->
                        {error, Reason}
                    end, {ok, State}, Steps),
  case Res of
    {ok, State2} ->
      {ok, State2};
    {error, Reason} ->
      {stop, Reason}
  end.

terminate(Reason, State = #{socket := Socket}) ->
  send(pgc_proto:encode_terminate_msg(), State),
  gen_tcp:close(Socket),
  ?LOG_INFO("connection closed"),
  terminate(Reason, maps:remove(socket, State));
terminate(_Reason, _State) ->
  ok.

handle_call({simple_query, Query}, _From, State) ->
  Result = send_simple_query(Query, State),
  {reply, Result, State};

handle_call({extended_query, Query, Params, Options}, _From, State) ->
  Result = send_extended_query(Query, Params, Options, State),
  {reply, Result, State};

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info({tcp_closed, _Socket}, State) ->
  {stop, connection_closed, State};

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec connect(state()) -> {ok, state()} | {error, term()}.
connect(State = #{options := Options}) ->
  Host0 = maps:get(host, Options, "localhost"),
  Host = unicode:characters_to_list(Host0),
  Port = maps:get(port, Options, 5432),
  TCPOptions = maps:get(tcp_options, Options, []),
  ?LOG_INFO("connecting to ~s:~b", [Host, Port]),
  TCPOptions2 = [{active, false}, {mode, binary}] ++ TCPOptions,
  case gen_tcp:connect(Host, Port, TCPOptions2) of
    {ok, Socket} ->
      ?LOG_INFO("connection established"),
      State2 = State#{socket => Socket},
      {ok, State2};
    {error, Reason} ->
      ?LOG_ERROR("connection failed: ~p", [Reason]),
      {error, {connect, Reason}}
  end.

-spec maybe_tls_connect(state()) -> {ok, state()} | {error, term()}.
maybe_tls_connect(#{options := Options} = State) ->
  case maps:get(tls, Options, false) of
    true ->
      tls_connect(State);
    false ->
      {ok, State}
  end.

-spec tls_connect(state()) -> {ok, state()} | {error, term()}.
tls_connect(State = #{options := Options, socket := Socket}) ->
  TLSOptions = maps:get(tls_options, Options, []),
  send(pgc_proto:encode_ssl_request_msg(), State),
  case gen_tcp:recv(Socket, 1) of
    {ok, <<"S">>} ->
      ?LOG_INFO("initializing tls connection"),
      case ssl:connect(Socket, TLSOptions) of
        {ok, SSLSocket} ->
          ?LOG_INFO("tls connection established"),
          {ok, State#{ssl_socket => SSLSocket}};
        {error, Reason} ->
          ?LOG_ERROR("tls connection failed: ~p", [Reason]),
          {error, {connect, Reason}}
      end;
    {ok, <<"N">>} ->
      ?LOG_ERROR("server does not support tls connections"),
      {error, missing_tls_support}
  end.

-spec begin_startup(state()) -> {ok, state()} | {error, term()}.
begin_startup(State = #{options := Options}) ->
  User = maps:get(user, Options),
  Database = maps:get(database, Options),
  ApplicationName = maps:get(application_name, Options, <<"erl-pgc">>),
  Parameters = #{user => User,
                 database => Database,
                 application_name => ApplicationName},
  send(pgc_proto:encode_startup_msg(3, 0, Parameters), State),
  {ok, State}.

-spec authenticate(state()) -> {ok, state()} | {error, term()}.
authenticate(State = #{options := Options}) ->
  User = maps:get(user, Options),
  Password = maps:get(password, Options, undefined),
  case recv_msg(State) of
    {error_response, Error} ->
      log_backend_error(Error),
      {error, Error};
    {notice_response, Notice} ->
      log_backend_notice(Notice, Options),
      authenticate(State);
    authentication_cleartext_password ->
      case Password of
        undefined ->
          {error, missing_password};
        _ ->
          send(pgc_proto:encode_password_msg(Password), State),
          authenticate(State)
      end;
    authentication_gss ->
      {error, gss_authentication_unsupported};
    authentication_kerberos_v5 ->
      {error, kerberos_v5_authentication_unsupported};
    {authentication_md5_password, Salt} ->
      case Password of
        undefined ->
          {error, missing_password};
        _ ->
          Hash = pgc_auth:hash_password_md5(User, Password, Salt),
          send(pgc_proto:encode_password_msg(Hash), State),
          authenticate(State)
      end;
    authentication_scm_credential ->
      {error, scm_authentication_unsupported};
    authentication_sspi ->
      {error, sspi_authentication_unsupported};
    {authentication_sasl, _Mechanisms} ->
      {error, sasl_authentication_unsupported};
    authentication_ok ->
      {ok, State};
    Msg ->
      {error, {unexpected_msg, Msg}}
  end.

-spec finish_startup(state()) -> {ok, state()} | {error, term()}.
finish_startup(State = #{options := Options}) ->
  case recv_msg(State) of
    {error_response, Error} ->
      log_backend_error(Error),
      {error, Error};
    {notice_response, Notice} ->
      log_backend_notice(Notice, Options),
      finish_startup(State);
    {parameter_status, _, _} ->
      finish_startup(State);
    {backend_key_data, _, _} ->
      finish_startup(State);
    {ready_for_query, _} ->
      {ok, State};
    Msg ->
      {error, {unexpected_msg, Msg}}
  end.

-spec send_simple_query(Query :: iodata(), state()) -> pgc:exec_result().
send_simple_query(Query, State) ->
  send(pgc_proto:encode_query_msg(Query), State),
  case recv_simple_query_response(State, pgc_proto:query_response()) of
    {ok, Response} ->
      pgc_proto:query_response_to_exec_result(Response);
    {error, Reason} ->
      {error, Reason}
  end.

-spec recv_simple_query_response(state(), pgc_proto:query_response()) ->
        {ok, Response :: map()} | {error, term()}.
recv_simple_query_response(State = #{options := Options}, Response) ->
  case recv_msg(State) of
    {error_response, Error} ->
      log_backend_error(Error),
      Response2 = maps:put(error, Error, Response),
      recv_simple_query_response(State, Response2),
      {error, Error};
    {notice_response, Notice} ->
      log_backend_notice(Notice, Options),
      recv_simple_query_response(State, Response);
    {parameter_status, _, _} ->
      recv_simple_query_response(State, Response);
    empty_query_response ->
      recv_simple_query_response(State, Response);
    {row_description, Columns} ->
      Response2 = maps:put(columns, Columns, Response),
      recv_simple_query_response(State, Response2);
    no_data ->
      Response2 = Response#{columns => [], rows => []},
      recv_simple_query_response(State, Response2);
    {data_row, Row} ->
      Response2 = pgc_proto:add_query_response_row(Row, Response),
      recv_simple_query_response(State, Response2);
    {command_complete, Tag} ->
      Response2 = maps:put(command_tag, Tag, Response),
      recv_simple_query_response(State, Response2);
    {ready_for_query, TransactionStatus} ->
      Response2 = maps:put(transaction_status, TransactionStatus, Response),
      case maps:find(error, Response) of
        {ok, Error} ->
          {error, Error};
        error ->
          {ok, Response2}
      end;
    Msg ->
      {error, {unexpected_msg, Msg}}
  end.

-spec send_extended_query(Query :: iodata(), Params :: [term()],
                          pgc:query_options(), state()) ->
        pgc:query_result().
send_extended_query(Query, Params, QueryOptions, State) ->
  #{options := Options} = State,
  Types = maps:get(types, Options, pgc_types:empty_type_set()),
  {EncodedParams, ParamTypeOids} = pgc_types:encode_values(Params, Types),
  send([pgc_proto:encode_parse_msg(<<>>, Query, ParamTypeOids),
        pgc_proto:encode_bind_msg(<<>>, <<>>, EncodedParams),
        pgc_proto:encode_describe_msg(portal, <<>>),
        pgc_proto:encode_execute_msg(<<>>, 0),
        pgc_proto:encode_sync_msg()],
       State),
  case recv_extended_query_response(State, pgc_proto:query_response()) of
    {ok, Response} ->
      pgc_proto:query_response_to_query_result(Response, Types, QueryOptions);
    {error, Reason} ->
      {error, Reason}
  end.

-spec recv_extended_query_response(state(), pgc_proto:query_response()) ->
        {ok, Response :: map()} | {error, term()}.
recv_extended_query_response(State = #{options := Options}, Response) ->
  case recv_msg(State) of
    {error_response, Error} ->
      log_backend_error(Error),
      Response2 = maps:put(error, Error, Response),
      recv_simple_query_response(State, Response2),
      {error, Error};
    {notice_response, Notice} ->
      log_backend_notice(Notice, Options),
      recv_extended_query_response(State, Response);
    {parameter_status, _, _} ->
      recv_extended_query_response(State, Response);
    empty_query_response ->
      recv_extended_query_response(State, Response);
    parse_complete ->
      recv_extended_query_response(State, Response);
    bind_complete ->
      recv_extended_query_response(State, Response);
    {row_description, Columns} ->
      Response2 = maps:put(columns, Columns, Response),
      recv_extended_query_response(State, Response2);
    no_data ->
      Response2 = Response#{columns => [], rows => []},
      recv_extended_query_response(State, Response2);
    {data_row, Row} ->
      Response2 = pgc_proto:add_query_response_row(Row, Response),
      recv_extended_query_response(State, Response2);
    {command_complete, Tag} ->
      Response2 = maps:put(command_tag, Tag, Response),
      recv_extended_query_response(State, Response2);
    {ready_for_query, TransactionStatus} ->
      Response2 = maps:put(transaction_status, TransactionStatus, Response),
      case maps:find(error, Response) of
        {ok, Error} ->
          {error, Error};
        error ->
          {ok, Response2}
      end;
    Msg ->
      {error, {unexpected_msg, Msg}}
  end.

-spec send(iodata(), state()) -> ok.
send(Data, #{ssl_socket := Socket}) ->
  ok = ssl:send(Socket, Data),
  ok;
send(Data, #{socket := Socket}) ->
  ok = gen_tcp:send(Socket, Data),
  ok.

-spec recv(integer(), state()) -> binary().
recv(Length, #{ssl_socket := Socket}) ->
  {ok, Data} = ssl:recv(Socket, Length),
  Data;
recv(Length, #{socket := Socket}) ->
  {ok, Data} = gen_tcp:recv(Socket, Length),
  Data.

-spec recv_msg(state()) -> pgc_proto:msg().
recv_msg(State = #{options := Options}) ->
  Header = recv(5, State),
  <<Type:8/integer, Size:32/integer>> = Header,
  Payload = case Size - 4 of
              0 -> <<>>;
              N -> recv(N, State)
            end,
  Msg = pgc_proto:decode_msg(Type, Payload),
  maps:get(log_messages, Options, false) andalso
    ?LOG_DEBUG("received message ~p", [Msg]),
  Msg.

-spec log_backend_notice(pgc:notice(), options()) -> ok.
log_backend_notice(Notice, Options) ->
  case maps:get(log_backend_notices, Options, true) of
    true ->
      #{code := Code, message := Message} = Notice,
      ?LOG_NOTICE("backend notice (code ~s): ~s", [Code, Message]),
      ok;
    false ->
      ok
  end.

-spec log_backend_error(pgc:error()) -> ok.
log_backend_error(Error) ->
  #{code := Code, message := Message} = Error,
  ?LOG_ERROR("backend error (code ~s): ~s", [Code, Message]),
  ok.
