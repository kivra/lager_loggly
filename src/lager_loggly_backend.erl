%% ----------------------------------------------------------------------------
%%
%% lager_loggly: Loggly backend for Lager
%%
%% Copyright (c) 2012 KIVRA
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.
%%
%% ----------------------------------------------------------------------------

-module(lager_loggly_backend).

-behaviour(gen_event).

-export([
         init/1
         ,handle_call/2
         ,handle_event/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

%%% this is only exported for the spawn call
-export([deferred_log/3]).

-record(state, {
                identity        :: string()
                ,level          :: integer()
                ,retry_interval :: integer()
                ,retry_times    :: integer()
                ,loggly_url     :: string()
               }).

-include_lib("lager/include/lager.hrl").

init([Identity, Level, RetryTimes, RetryInterval, LogglyUrl]) ->
    State = #state{
                   identity        = Identity
                   ,level          = lager_util:level_to_num(Level)
                   ,retry_interval = RetryInterval
                   ,retry_times    = RetryTimes
                   ,loggly_url     = LogglyUrl
                  },
    {ok, State}.

handle_call(get_loglevel, #state{ level = Level } = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    {ok, ok, State#state{ level = lager_util:level_to_num(Level) }};
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event({log, Level, {_Date, _Time}, [_LevStr, Loc, Message]}, State) ->
    Payload = jsx:to_json([
                            {<<"identity">>, State#state.identity}
                            ,{<<"level">>, convert_level(Level)}
                            ,{<<"location">>, list_to_binary(Loc)}
                            ,{<<"message">>, list_to_binary(Message)}
                          ]),
    Request = {State#state.loggly_url, [], "application/json", Payload},
    RetryTimes = State#state.retry_times,
    RetryInterval = State#state.retry_interval,

    %% Spawn a background process to handle sending the email.
    %% It will recurse until the message is successfully sent.
    spawn(?MODULE, deferred_log, [Request, RetryTimes, RetryInterval]),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% Private


deferred_log(_Request, 0, _) ->
    ok;
deferred_log(Request, Retries, Interval) ->
    case httpc:request(post, Request, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _H, _B}} -> ok;
        _ ->
            timer:sleep(Interval * 1000),
            deferred_log(Request, Retries - 1, Interval)
    end.

convert_level(Level) ->
    list_to_binary(atom_to_list(lager_util:num_to_level(Level))).
