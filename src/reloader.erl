-module(reloader).

-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).
-export([start/0, start/1, start_link/0, start_link/1]).
-export([stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([all_changed/0]).
-export([is_changed/1]).
-export([reload_modules/1]).
-record(state, {last, tref}).

%% External API

%% @spec start() -> ServerRet
%% @doc Start the reloader in autoreload mode.
start() ->
    start(auto).

%% @spec start(auto) -> ServerRet
%% @doc Start the reloader in autoreload mode.
start(auto) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [auto], []);

%% @spec start(manual) -> ServerRet
%% @doc Start the reloader in manual mode. Updates need to be triggered
start(manual) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [manual], []).

%% @spec start_link() -> ServerRet
%% @doc Start the reloader in autoreload mode.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [auto], []).

%% @spec start_link(auto) -> ServerRet
%% @doc Start the reloader in autoreload mode.
start_link(auto) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [auto], []);

%% @spec start_link(auto) -> ServerRet
%% @doc Start the reloader in manual mode. Updates need to be triggered.
start_link(manual) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [manual], []).

%% @spec stop() -> ok
%% @doc Stop the reloader.
stop() ->
    gen_server:call(?MODULE, stop).

%% gen_server callbacks

%% @spec init([Mode]) -> {ok, State}
%% @doc gen_server init, opens the server in an initial state.
init([Mode]) ->
    case Mode of
        auto ->
            {ok, TRef} = timer:send_interval(timer:seconds(1), doit),
            {ok, #state{last = stamp(), tref = TRef}};
        manual ->
            {ok, #state{}};
        _ ->
            error_logger:error_msg("Invalid start argument"),
            {stop, "Invalid start argument: ~p. Use auto/manual~n~n", [Mode]}
    end.

%% @spec handle_call(Args, From, State) -> tuple()
%% @doc gen_server callback.
handle_call(stop, _From, State) ->
    {stop, shutdown, stopped, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.

%% @spec handle_cast(Cast, State) -> tuple()
%% @doc gen_server callback.
handle_cast(_Req, State) ->
    {noreply, State}.

%% @spec handle_info(Info, State) -> tuple()
%% @doc gen_server callback.
handle_info(doit, State) ->
    Now = stamp(),
    _ = doit(State#state.last, Now),
    {noreply, State#state{last = Now}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> ok
%% @doc gen_server termination callback.
terminate(_Reason, State) ->
    {ok, cancel} = timer:cancel(State#state.tref),
    ok.


%% @spec code_change(_OldVsn, State, _Extra) -> State
%% @doc gen_server code_change callback (trivial).
code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%% @spec reload_modules([atom()]) -> [{module, atom()} | {error, term()}]
%% @doc code:purge/1 and code:load_file/1 the given list of modules in order,
%%      return the results of code:load_file/1.
reload_modules(Modules) ->
    [begin code:purge(M), code:load_file(M) end || M <- Modules].

%% @spec all_changed() -> [atom()]
%% @doc Return a list of beam modules that have changed.
all_changed() ->
    [M || {M, Fn} <- code:all_loaded(), is_list(Fn), is_changed(M)].

%% @spec is_changed(atom()) -> boolean()
%% @doc true if the loaded module is a beam with a vsn attribute
%%      and does not match the on-disk beam file, returns false otherwise.
is_changed(M) ->
    try
        module_vsn(M:module_info()) =/= module_vsn(code:get_object_code(M))
    catch _:_ ->
            false
    end.

%% Internal API

module_vsn({M, Beam, _Fn}) ->
    {ok, {M, Vsn}} = beam_lib:version(Beam),
    Vsn;
module_vsn(L) when is_list(L) ->
    {_, Attrs} = lists:keyfind(attributes, 1, L),
    {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
    Vsn.

doit(From, To) ->
    [case file:read_file_info(Filename) of
         {ok, #file_info{mtime = Mtime}} when Mtime >= From, Mtime < To ->
             reload(Module);
         {ok, _} ->
             unmodified;
         {error, enoent} ->
             %% The Erlang compiler deletes existing .beam files if
             %% recompiling fails.  Maybe it's worth spitting out a
             %% warning here, but I'd want to limit it to just once.
             gone;
         {error, Reason} ->
             lager:info("Error reading ~s's file info: ~p",
                       [Filename, Reason]),
             error
     end || {Module, Filename} <- code:all_loaded(), is_list(Filename)].

reload(Module) ->
    error_logger:info_msg("Reloading ~p ...", [Module]),
    code:purge(Module),
    case code:load_file(Module) of
        {module, Module} ->
            lager:info(" ok."),
            reload;
        {error, Reason} ->
            lager:error(" fail: ~p", [Reason]),
            error
    end.


stamp() ->
    erlang:localtime().
