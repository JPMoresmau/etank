-module(tank_runner_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/tank_records.hrl").


sitting_duck_test_() ->
    Tank = tank_demo:sitting_duck(),
    TRef = tank_runner:make_tank_ref(Tank),
    {Ret0,Pid0} = tank_runner:start_link(TRef),
    TankRef = TRef#tank_ref.ref,
    TickRef1 = make_ref(),
    gen_server:cast(Pid0,{tick,self(),TickRef1,world}),
    Rec1 = receive
            {_From1,R1} -> R1
            after 500 -> undefined
        end,
    TickRef2 = make_ref(),
    gen_server:cast(Pid0,{tick,self(),TickRef2,world}),
    Rec2 = receive
            {_From2,R2} -> R2
            after 500 -> undefined
        end,
    gen_server:call(Pid0,terminate),
    [?_assertEqual('Sitting Duck',TRef#tank_ref.atom_name)
    ,?_assertEqual(ok,Ret0)
    ,?_assertEqual({action,{TickRef1,TankRef},nothing},Rec1)
    ,?_assertEqual({action,{TickRef2,TankRef},nothing},Rec2)
    ].


increment_test_() ->
    Fun = fun (State,_World) -> {State+1,State} end,
    Tank = #tank{name="Increment",state=0,update_function=Fun},
    TRef = tank_runner:make_tank_ref(Tank),
    {Ret0,Pid0} = tank_runner:start_link(TRef),
    TankRef = TRef#tank_ref.ref,
    TickRef1 = make_ref(),
    gen_server:cast(Pid0,{tick,self(),TickRef1,world}),
    Rec1 = receive
            {_From1,R1} -> R1
            after 500 -> undefined
        end,
    TickRef2 = make_ref(),
    gen_server:cast(Pid0,{tick,self(),TickRef2,world}),
    Rec2 = receive
            {_From2,R2} -> R2
            after 500 -> undefined
        end,
    gen_server:call(Pid0,terminate),
    [?_assertEqual('Increment',TRef#tank_ref.atom_name)
    ,?_assertEqual(ok,Ret0)
    ,?_assertEqual({action,{TickRef1,TankRef},0},Rec1)
    ,?_assertEqual({action,{TickRef2,TankRef},1},Rec2)
    ].