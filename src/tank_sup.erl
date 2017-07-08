%%% supervisor for all tanks processes
-module(tank_sup).
-behavior(supervisor).

-export([start_link/0,add_tank/1,remove_tank/1,get_pids/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init(_Args) ->
    {ok, {{simple_one_for_one, 3, 60},
        [{tank,
        {tank_runner, start_link, []},
        transient, 1000, worker, [tank_runner]}
        ]}}.

add_tank(TankRef) ->
    supervisor:start_child(?MODULE, [TankRef]).

remove_tank(Pid) ->
    supervisor:terminate_child(?MODULE,Pid).

get_pids() ->
    lists:map(fun(T)->element(2,T) end,supervisor:which_children(?MODULE)).
