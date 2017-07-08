%%% Show the current game in a "graphical" board
-module(tank_board).

-behaviour(gen_event).
-include("tank_records.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
   terminate/2]).

init([]) ->
    {ok, []}.

handle_event({map, Map}, State) ->
    {MX,MY} = Map#tank_map.size,
    % Tanks sorted by index
    Tanks = lists:sort(fun(T1,T2) -> T1#tank_info.index =< T2#tank_info.index end ,gb_trees:values(Map#tank_map.tanks)),
    % Tanks keyed by position
    TTPos = lists:foldl(fun(TI,T) -> gb_trees:insert(TI#tank_info.pos,TI,T) end,gb_trees:empty(),Tanks),
    TL = lists:concat([
        "Tanks:~n",
        lists:concat(lists:map(fun (T)->io_lib:format("~p: ~p (~p)~n",[T#tank_info.index,T#tank_info.atom_name,T#tank_info.health]) end,Tanks))    
        ]),
    % Show list of tank number/name
    io:format(TL,[]),
    % show world
    M = lists:concat([
        "~n",
        lists:concat(lists:map(fun(_X)-> "_" end,lists:seq(1, MX+1))),
        "_~n",
        lists:concat(lists:map(fun(Y)-> write_line(Map,TTPos,Y) end,lists:seq(1, MY))),
        lists:concat(lists:map(fun(_X)-> "_" end,lists:seq(1, MX+1))),
        "_~n"
        ]),
    io:format(M,[]),
    %io:format("Map: ~p~n",[Map]),
    {ok, State};
handle_event({dead,TankName}, State) ->
    io:format("Tank ~p is Dead!~n",[TankName]),
    {ok, State};
handle_event({winner,Tank}, State) ->
    io:format("Tank ~p wins with ~p health remaining!~n",[Tank#tank_info.atom_name,Tank#tank_info.health]),
    {ok, State};
handle_event({action,TankName,{fire,Tgt,true}}, State) ->
    io:format("Tank ~p fires at ~p and hits!~n",[TankName,Tgt]),
    {ok, State};
handle_event({action,TankName,{fire,Tgt,false}}, State) ->
    io:format("Tank ~p fires at ~p and misses!~n",[TankName,Tgt]),
    {ok, State};
handle_event({action,TankName,{_A,Tgt}}, State) ->
    io:format("Tank ~p moves to ~p.~n",[TankName,Tgt]),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
terminate(_Reason, _State) ->
    ok.

%% write one line in the map
write_line(Map,TTPos, Y) ->
    {MX,_MY} = Map#tank_map.size,
    lists:concat(["|",
        lists:map(fun(X)-> get_point(X,Y,TTPos) end,lists:seq(1, MX)),
        "|~n"]).

%% get the representation of one point on the map(either a space or a tank number)
get_point(X,Y,TTPos) ->
    case gb_trees:lookup({X-1,Y-1},TTPos) of
        none -> " ";
        {value,TI} -> integer_to_list(TI#tank_info.index)
    end.