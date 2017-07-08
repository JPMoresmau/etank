-module(tank_sup_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/tank_records.hrl").


sitting_duck_test_() ->
    Tank = tank_demo:sitting_duck(),
    TRef = tank_runner:make_tank_ref(Tank),
    {Ret0,Pid0} = tank_sup:start_link(),
    {Ret1,Pid1} = tank_sup:add_tank(TRef),
    PidPresent1 = lists:member(Pid1,tank_sup:get_pids()),
    Ret2 = tank_sup:remove_tank(Pid1),
    PidPresent2 = lists:member(Pid1,tank_sup:get_pids()),
    Ref = monitor(process, Pid0),
    unlink(Pid0),
    exit(Pid0,shutdown),
    receive
        {'DOWN', Ref, process, Pid0, _Reason} ->
            ok
    after 1000 ->
            error(exit_timeout)
    end,
    [ ?_assertEqual(ok,Ret0)
    , ?_assertEqual(ok,Ret1)
    , ?_assertEqual(ok,Ret2)
    , ?_assert(PidPresent1)
    , ?_assertEqual(false,PidPresent2)
    ].