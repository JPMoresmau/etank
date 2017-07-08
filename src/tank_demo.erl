%%% Demo world
-module(tank_demo).
-export([sitting_duck/0,headless_chicken/0,demo_world/0]).
-include("tank_records.hrl").

%% this tank does nothing, just stays immobile
sitting_duck() ->
    Fun = fun (State,_World) -> {State,nothing} end,
    #tank{name="Sitting Duck",state=no_state,update_function=Fun}.

%% this tank moves randomly
headless_chicken() ->
    Fun = fun (State,_World) -> {State,random_move()} end,
    #tank{name="Headless Chicken",state=no_state,update_function=Fun}.

%% this tank fires at the closest tank if in range, otherwise moves towards the nearest tank
rabid_dog() ->
    #tank{name="Rabid Dog",state='Rabid Dog',update_function=fun rabid/2}.

%% rabid dog tank update function
rabid(State,World)  ->
    MyName = tank_runner:ensure_atom(State),
    case tank_rules:closer(World,MyName) of
        none -> {State,random_move()};
        {TankInfo,_,InRange} -> 
            Pos = TankInfo#tank_info.pos,
            case InRange of
                true -> {State,{fire,Pos}};
                false -> {State,tank_rules:move_toward(World,MyName,Pos)}
            end
end. 

%% a random move
random_move() ->
    lists:nth(rand:uniform(4),[move_north,move_east,move_south,move_west]).

%% build and start demo world with all demo tanks
demo_world() ->
    {ok,Pid} = tank_world:start_world(),
    tank_world:add_tank(Pid,sitting_duck()),
    tank_world:add_tank(Pid,headless_chicken()),
    tank_world:add_tank(Pid,rabid_dog()),
    {ok,Pid}.