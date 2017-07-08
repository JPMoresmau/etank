%%% The process handling a specific tank
-module(tank_runner).
-behavior(gen_server).
-export([start_link/1,make_tank_ref/1,ensure_atom/1]).
-export([init/1,handle_call/3,handle_info/2,handle_cast/2,code_change/3,terminate/2]).
-include("tank_records.hrl").

%% build a tank reference record from a Tank
make_tank_ref(Tank) -> 
    Ref = make_ref(),
    AN = ensure_atom(Tank#tank.name),
    #tank_ref{atom_name=AN,ref=Ref,tank=Tank}.

%% start the process from a tank reference
start_link(TankRef) -> 
    gen_server:start_link({local,TankRef#tank_ref.atom_name},?MODULE, [TankRef], []).
    
%% ensure given value is an atom
ensure_atom( T ) when is_atom(T) -> T;
ensure_atom( T ) when is_list(T) -> list_to_atom(T).

init([State]) -> {ok,State}. 

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.


handle_cast({tick,Pid,RefTick,World}, T=#tank_ref{ref=RefTank,
    tank=#tank{name=Name,state=State,update_function=Fun}}) ->
        %io:format("~s received World: ~p~n",[Name,World]),
        %% run the Tank update function and store new state
        {NewState,Action} = Fun(State,World),
        gen_server:cast(Pid,{action,{RefTick,RefTank},Action}),
        %io:format("~s sent: ~p~n",[Name,Action]),
        {noreply, T#tank_ref{tank=#tank{name=Name,state=NewState,update_function=Fun}}}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 

terminate(_Reason,_State) ->
    ok.