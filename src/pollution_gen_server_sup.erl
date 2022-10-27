%%%-------------------------------------------------------------------
%% @doc pollution_gen_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pollution_gen_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => 'pollution_gen_server',
      start => {pollution_gen_server, start, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [pollution_gen_server]} ,
      #{id => 'pollution_value_collector_gen_statem',
        start => {pollution_value_collector_gen_statem, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [pollution_value_collector_gen_statem]} ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
