-module(tank_world_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/tank_records.hrl").

sanity_add_test_() ->
    {Ret0,Pid0} = tank_world:start_world(),
    Ret1 = tank_world:add_tank(Pid0,tank_demo:sitting_duck()),
    tank_world:stop_world(Pid0),
    [ ?_assertEqual(ok,Ret0)
    , ?_assertEqual(ok,Ret1)
    ]
    .