%%% Implement the rules about moving and firing
-module(tank_rules).
-export([spawn_tank/2,apply_action/3,winner/1,remove_dead/1,other_ranges/2,closer/2,move_toward/3]).
-include("tank_records.hrl").

-define(MAX_RANGE,5).

%% spawn a new tank in a new random location
spawn_tank(Map,TankName) ->
    {MX,MY} = Map#tank_map.size,
    {NX,NY} ={rand:uniform(MX)-1,rand:uniform(MY)-1},
    case lists:member({NX,NY},other_positions(Map,TankName)) of
        true -> spawn_tank(Map,TankName);
        _ -> 
            TankInfo = #tank_info{atom_name=TankName,pos={NX,NY}, index=get_index(Map#tank_map.tanks)},
            Map#tank_map{tanks=gb_trees:enter(TankName,TankInfo,Map#tank_map.tanks)}
    end.

%% get the next available index in the tank map
get_index(Tanks) ->
    get_index(1,lists:map(fun(T) -> T#tank_info.index end, gb_trees:values(Tanks))).

%% get the next available index in the list of indices
get_index(Idx,Idxs) -> 
    case lists:member(Idx,Idxs) of
        true -> get_index(Idx+1,Idxs);
        false -> Idx 
    end.

%% apply a tank action
apply_action(Map,TankName,move_north) ->
    move(Map,TankName,move_north,0,-1);
apply_action(Map,TankName,move_east) ->
    move(Map,TankName,move_east,1,0);
apply_action(Map,TankName,move_south) ->
    move(Map,TankName,move_south,0,1);
apply_action(Map,TankName,move_west) ->
    move(Map,TankName,move_west,-1,0);
apply_action(Map,TankName,{fire,Tgt}) ->
    fire(Map,TankName,fire,Tgt);
apply_action(Map,_TankName,_Action) ->
    {Map,nothing}.

%% move tank by given deltas
move(Map,TankName,Action,XDiff,YDiff) ->
    case gb_trees:lookup(TankName,Map#tank_map.tanks) of
        none -> {Map,nothing};
        {value,TankInfo} -> move(Map,TankName,TankInfo,Action,XDiff,YDiff)
    end.

%% move tank by given delta, with the full tank info
move(Map,TankName,TankInfo,Action,XDiff,YDiff) ->
    {X,Y} = TankInfo#tank_info.pos,
    {NX,NY} = {X + XDiff, Y + YDiff},
    {MX,MY} = Map#tank_map.size,
    case NX<0 orelse NX>=MX orelse NY<0 orelse NY>=MY orelse lists:member({NX,NY},other_positions(Map,TankName)) of
        true ->
            {Map,nothing};
        _ -> 
            TankInfo2 = TankInfo#tank_info{pos={NX,NY}},
            Map2 = Map#tank_map{tanks=gb_trees:enter(TankName,TankInfo2,Map#tank_map.tanks)},
            {Map2,TankName,{Action,{NX,NY}}}
    end.

%% fire on given position
fire(Map,TankName,Action,Tgt) ->
    case gb_trees:lookup(TankName,Map#tank_map.tanks) of
        none -> {Map,nothing};
        {value,TankInfo} -> fire(Map,TankName,TankInfo,Action,Tgt)
    end.

%% fire on given position, with the full tank info
fire(Map,TankName,TankInfo,Action,Tgt) ->
    case in_range(TankInfo#tank_info.pos,Tgt) of
        false -> {Map,nothing};
        true -> 
            NTanks = gb_trees:map(fun (_K,V) -> fire(V,Tgt) end,Map#tank_map.tanks),
            Map2 = Map#tank_map{tanks=NTanks},
            {Map2,TankName,{Action,Tgt,Map2=/=Map}}
        end.

%% calculate if a tank is the target of fire
fire(TankInfo,Tgt) when TankInfo#tank_info.pos =:= Tgt
    -> TankInfo#tank_info{health=TankInfo#tank_info.health-1};
fire(TankInfo,_Tgt) -> TankInfo.

%% distance between two positions
distance({X1,Y1},{X2,Y2}) -> math:sqrt(math:pow(X1-X2,2)+math:pow(Y1-Y2,2)).

%% are we in range of fire
in_range(Pos1,Pos2) -> distance(Pos1,Pos2) < ?MAX_RANGE.

%% other tanks
others(Map,TankName) ->
    Tanks = gb_trees:values(Map#tank_map.tanks),
    lists:filter(fun (T) ->T#tank_info.atom_name =/=TankName end, Tanks).

%% other tanks positions
other_positions(Map,TankName) ->
    lists:map(fun(T) -> T#tank_info.pos end,others(Map,TankName)).

%% other tanks with their distance and are they in range
other_ranges (Map,TankName) ->
    case gb_trees:lookup(TankName,Map#tank_map.tanks) of
        none -> [];
        {value,TankInfo} -> 
            Tanks = others(Map,TankName),
            lists:map(fun(T) -> {T,distance(TankInfo#tank_info.pos,T#tank_info.pos),in_range(TankInfo#tank_info.pos,T#tank_info.pos)} end, Tanks)
    end.
    
%% find closer tank
closer (Map,TankName) ->
    case other_ranges(Map,TankName) of
        [] -> 
            %io:format("closer -> none~n",[]),
            none;
        List -> 
            Res = hd(lists:sort(fun({_,D1,_},{_,D2,_}) -> D1=<D2 end, List)),
            %io:format("close -> ~p~n",[Res]),
            Res
    end.

%% move toward the given posiion
move_toward(Map,TankName,{XT,YT}) ->
    case gb_trees:lookup(TankName,Map#tank_map.tanks) of
        none -> nothing;
        {value,TankInfo} -> 
            {X,Y} = TankInfo#tank_info.pos,
            case {X<XT,X>XT,Y<YT,Y>YT} of
                {true,_,_,_} -> move_east;
                {_,true,_,_} -> move_west;
                {_,_,true,_} -> move_south;
                {_,_,_,true} -> move_north
            end
    end.

%% determine the winner if any
winner(Map) ->
    Tanks = gb_trees:values(Map#tank_map.tanks),
    Alive = lists:filter(fun (T) ->T#tank_info.health>0 end, Tanks),
    winner(Tanks,Alive).

%% determine the winner from the full list of tanks and list of alive tanks
winner(Tanks,Alive) ->
    case length(Tanks)>1 of % if we had more than one tank at the start of the round
        true -> case length(Alive) of
            0 -> none;
            1 -> hd(Alive);
        _ -> not_finished
    end;
    _ -> none
end.

%% remove dead tanks from the map and return the winner information
remove_dead(Map) ->
    Tanks = gb_trees:values(Map#tank_map.tanks),
    {Dead,Alive} = lists:partition(fun (T) ->T#tank_info.health<1 end, Tanks),
    DeadNames = lists:map(fun (T) ->T#tank_info.atom_name end,Dead),
    Map2 = Map#tank_map{tanks=lists:foldl(fun(N,T)->gb_trees:delete(N,T) end,Map#tank_map.tanks,DeadNames)},
    Winner =  winner(Tanks,Alive),
    {Map2,DeadNames,Winner}.
