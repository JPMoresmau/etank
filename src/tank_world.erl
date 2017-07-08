%%% handles updating the world regularly and sending events to say what happened
-module(tank_world).
-behavior(gen_server).

-export([start_world/0,add_tank/2,stop_world/1]).
-export([init/1,handle_call/3,handle_info/2,handle_cast/2,code_change/3,terminate/2]).

-include("tank_records.hrl").

-define(INTERVAL, 1000).

%% the state we keep
-record(tank_state,{
        max_tanks = 10,
        tanks = gb_trees:empty(), % tanks references by name
        map = #tank_map{}, % the world
        tanks_tick = gb_trees:empty(), % tanks that still have to send an action for this tick
        last_tick = undefined, % reference generated each tick
        event_pid = undefined
    }).

%% start a new blank world
start_world() -> 
    {ok, Pid} = gen_event:start_link(),
    gen_event:add_handler(Pid, tank_board, []),
    TS = #tank_state{event_pid=Pid},
    gen_server:start_link({local,?MODULE},?MODULE, [TS], []).

%% add a tank
add_tank(Pid,Tank)-> 
    gen_server:call(Pid,{add,Tank}).

%% stop the world
stop_world(Pid) ->
    gen_server:call(Pid,terminate). 

%% start ticking
init([State]) -> 
    tank_sup:start_link(),
    erlang:send_after(?INTERVAL, self(), trigger),
    {ok,State}. 

%% add a new tank
handle_call({add,Tank},_From,State) -> 
    case gb_trees:size(State#tank_state.tanks)>=State#tank_state.max_tanks of
        true -> {reply,too_many_tanks,State};
        _ ->
            TankRef = tank_runner:make_tank_ref(Tank), % create ref
            Key = TankRef#tank_ref.atom_name, % name is the key in the map
            case gb_trees:is_defined(Key,State#tank_state.tanks) of
                true ->
                    {reply,already_exist,State};
                false ->
                    {ok,_Pid} = tank_sup:add_tank(TankRef), % add process
                    NTanks = gb_trees:enter(Key,TankRef,State#tank_state.tanks), % tank by key
                    NMap = tank_rules:spawn_tank(State#tank_state.map,Key), % position and index are generated
                    {reply,ok,State#tank_state{tanks=NTanks,map=NMap}}
            end
        end;
handle_call(terminate, _From, State) ->
        Pids = tank_sup:get_pids(),
        lists:foreach(fun (P) -> tank_sup:remove_tank(P) end,Pids), % stop tanks
        {stop, normal, ok, State}.

handle_cast({action,{TickRef,TankRef},Action}, State) when TickRef =:= State#tank_state.last_tick -> % only deal with current tick action
    TanksTick=State#tank_state.tanks_tick,

    case gb_trees:lookup(TankRef,TanksTick) of
        {value,TankName} -> % ensure tank has not already sent an action for this tick
            %io:format("Received Action: ~p: ~p~n",[TankRef,Action]),
            TanksTick2 = gb_trees:delete(TankRef,TanksTick),
            % ask the rules to process the action
            {NMap,Finish} = case tank_rules:apply_action( State#tank_state.map,TankName,Action) of
                {Map,TankName,Result} -> 
                    % notify of action
                    gen_event:notify( State#tank_state.event_pid, {action, TankName,Result}),
                    % are we finished?
                    check_end(State,Map);
                _ ->  {State#tank_state.map,false}
                end,
            % new state
            NState = State#tank_state{tanks_tick=TanksTick2,map=NMap},
            case Finish of
                true -> 
                    {stop, normal, NState}; % stop!
                _ -> 
                    gen_event:notify( State#tank_state.event_pid, {map, NMap}), % notify of new map
                    {noreply, NState} % continue
            end;
        _ -> {noreply, State}
    end;
handle_cast(_,State) -> 
    {noreply, State}.

handle_info(trigger,State) ->
    NewState = tick(State),
    {noreply,NewState};
handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 

terminate(_Reason,_State) ->
    ok.

%% one tick in the game
tick(State) ->
    TickRef = make_ref(), % specific ref for tick
    Pids = tank_sup:get_pids(), % get running tank pids
    %%io:format("Tick (~p tanks)~n",[length(Pids)]),
    % build tree of tank names and tank refs to keep track of which tanks have sent actions
    RefNames = lists:map(fun (T) -> {T#tank_ref.ref,T#tank_ref.atom_name} end,
        gb_trees:values(State#tank_state.tanks)),
    NewTanksTick = lists:foldl(fun({R,N},T) -> gb_trees:insert(R,N,T) end,gb_trees:empty(),RefNames),
    % send event
    lists:foreach(fun (P) -> tick1(State#tank_state.map,TickRef,P) end,Pids),
    % schedule next tick
    erlang:send_after(?INTERVAL, self(), trigger),
    State#tank_state{last_tick=TickRef,tanks_tick=NewTanksTick}.

%% send a tick event to the given Pid
tick1(World, TickRef, Pid) ->
    gen_server:cast(Pid,{tick,self(),TickRef,World}).

%% check if we ended
check_end(State, Map) ->
    {Map2,Deads,Winner} = tank_rules:remove_dead(Map), % remove dead
    lists:foreach(fun (T) -> gen_event:notify(State#tank_state.event_pid, {dead, T}) end,Deads),
    Finish = case Winner of 
        none -> true;
        not_finished -> false;
        Tank -> gen_event:notify( State#tank_state.event_pid, {winner, Tank}),
            true
    end,
    {Map2,Finish}.