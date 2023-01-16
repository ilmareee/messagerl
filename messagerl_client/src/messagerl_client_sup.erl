%%%-------------------------------------------------------------------
%% @doc messagerl_client top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(messagerl_client_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
	SupFlags = #{strategy => one_for_one,
					intensity => 3,
					period => 60},
	ChildSpecs = [#{id => messagerl_client_serv,
					start => {messagerl_client_serv, start_link, []},
					restart => permanent,
					shutdown => 1000,
					type => worker,
					modules => [messagerl_client_serv]}
					],
	{ok, {SupFlags, ChildSpecs}}.

