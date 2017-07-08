-module(tank_rules_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/tank_records.hrl").

winner_test_() ->
    T1 = #tank_info{atom_name='T1',health=10,pos={1,1}},
    T2 = #tank_info{atom_name='T2',health=10,pos={1,2}},
    T3 = #tank_info{atom_name='T3',health=0,pos={1,3}},
    TM1 = #tank_map{size={10,10},tanks=gb_trees:empty()},
    TM2 = #tank_map{size={10,10},tanks=gb_trees:from_orddict([{'T1',T1}])},
    TM3 = #tank_map{size={10,10},tanks=gb_trees:from_orddict([{'T1',T1},{'T2',T2}])},
    TM4 = #tank_map{size={10,10},tanks=gb_trees:from_orddict([{'T1',T1},{'T3',T3}])},
    [ ?_assertEqual(none,tank_rules:winner(TM1))
    , ?_assertEqual(none,tank_rules:winner(TM2))
    , ?_assertEqual(not_finished,tank_rules:winner(TM3))
    , ?_assertEqual(T1,tank_rules:winner(TM4))
    ].

remove_dead_test_() ->
    T1 = #tank_info{atom_name='T1',health=10,pos={1,1}},
    T2 = #tank_info{atom_name='T2',health=10,pos={1,2}},
    T3 = #tank_info{atom_name='T3',health=0,pos={1,3}},
    TM1 = #tank_map{size={10,10},tanks=gb_trees:from_orddict([{'T1',T1},{'T2',T2}])},
    TM2 = #tank_map{size={10,10},tanks=gb_trees:from_orddict([{'T1',T1},{'T3',T3}])},
    TM2After = #tank_map{size={10,10},tanks=gb_trees:from_orddict([{'T1',T1}])},
    [ ?_assertEqual({TM1,[],not_finished},tank_rules:remove_dead(TM1)),
    ?_assertEqual({TM2After,['T3'],T1},tank_rules:remove_dead(TM2))
    ].

move_border_test_() ->
    T1 = #tank_info{atom_name='T1',health=10,pos={0,0}},
    T2 = #tank_info{atom_name='T2',health=10,pos={9,9}},
    T3 = #tank_info{atom_name='T2',health=10,pos={9,0}},
    T4 = #tank_info{atom_name='T2',health=10,pos={0,9}},
    TM1 = #tank_map{size={10,10},tanks=gb_trees:from_orddict([{'T1',T1},{'T2',T2},{'T3',T3},{'T4',T4}])},
    [
    ?_assertEqual({TM1,nothing},tank_rules:apply_action(TM1,'T1',move_north)),
    ?_assertEqual({TM1,nothing},tank_rules:apply_action(TM1,'T1',move_west)),
    ?_assertEqual({TM1,nothing},tank_rules:apply_action(TM1,'T2',move_south)),
    ?_assertEqual({TM1,nothing},tank_rules:apply_action(TM1,'T2',move_east)),
    ?_assertEqual({TM1,nothing},tank_rules:apply_action(TM1,'T3',move_east)),
    ?_assertEqual({TM1,nothing},tank_rules:apply_action(TM1,'T3',move_north)),
    ?_assertEqual({TM1,nothing},tank_rules:apply_action(TM1,'T4',move_south)),
    ?_assertEqual({TM1,nothing},tank_rules:apply_action(TM1,'T4',move_west))
    ].
    
move_conflict_test_() ->
    T1 = #tank_info{atom_name='T1',health=10,pos={0,0}},
    T2 = #tank_info{atom_name='T2',health=10,pos={0,1}},
    TM1 = #tank_map{size={10,10},tanks=gb_trees:from_orddict([{'T1',T1},{'T2',T2}])},
    [
    ?_assertEqual({TM1,nothing},tank_rules:apply_action(TM1,'T1',move_south)),
    ?_assertEqual({TM1,nothing},tank_rules:apply_action(TM1,'T2',move_north))
    ].

move_test_() ->
    T1 = #tank_info{atom_name='T1',health=10,pos={0,0}},
    T2 = #tank_info{atom_name='T1',health=10,pos={1,0}},
    T3 = #tank_info{atom_name='T1',health=10,pos={1,1}},
    T4 = #tank_info{atom_name='T1',health=10,pos={0,1}},
    TM1 = #tank_map{size={10,10},tanks=gb_trees:from_orddict([{'T1',T1}])},
    TM2 = #tank_map{size={10,10},tanks=gb_trees:from_orddict([{'T1',T2}])},
    TM3 = #tank_map{size={10,10},tanks=gb_trees:from_orddict([{'T1',T3}])},
    TM4 = #tank_map{size={10,10},tanks=gb_trees:from_orddict([{'T1',T4}])},
    [
    ?_assertEqual({TM2,'T1',{move_east,{1,0}}},tank_rules:apply_action(TM1,'T1',move_east)),
    ?_assertEqual({TM3,'T1',{move_south,{1,1}}},tank_rules:apply_action(TM2,'T1',move_south)),
    ?_assertEqual({TM4,'T1',{move_west,{0,1}}},tank_rules:apply_action(TM3,'T1',move_west)),
    ?_assertEqual({TM1,'T1',{move_north,{0,0}}},tank_rules:apply_action(TM4,'T1',move_north))
    ].

spawn_test_() ->
    TM1 = #tank_map{size={10,10},tanks=gb_trees:empty()},
    TM2 = tank_rules:spawn_tank(TM1,'T1'),
    TM3 = tank_rules:spawn_tank(TM2,'T2'),
    T1 =gb_trees:get('T1',TM3#tank_map.tanks),
    T2 =gb_trees:get('T2',TM3#tank_map.tanks),
    {X1,Y1} = T1#tank_info.pos,
    {X2,Y2} = T2#tank_info.pos,
    [?_assert(X1>=0)
    ,?_assert(X1<10)
    ,?_assert(Y1>=0)
    ,?_assert(Y1<10)
    ,?_assert(X2>=0)
    ,?_assert(X2<10)
    ,?_assert(Y2>=0)
    ,?_assert(Y2<10)
    ,?_assertNotEqual({X1,Y1},{X2,Y2})
    ,?_assertEqual(1,T1#tank_info.index)
    ,?_assertEqual(2,T2#tank_info.index)
    ].

fire_test_() ->
    T1 = #tank_info{atom_name='T1',health=10,pos={0,0}},
    T2 = #tank_info{atom_name='T2',health=10,pos={1,0}},
    T2hurt = #tank_info{atom_name='T2',health=9,pos={1,0}},
    T3 = #tank_info{atom_name='T3',health=10,pos={10,10}},
    TM1 = #tank_map{size={10,10},tanks=gb_trees:from_orddict([{'T1',T1},{'T2',T2},{'T3',T3}])},
    TM2 = #tank_map{size={10,10},tanks=gb_trees:from_orddict([{'T1',T1},{'T2',T2hurt},{'T3',T3}])},
    [?_assertEqual({TM2,'T1',{fire,{1,0},true}},tank_rules:apply_action(TM1,'T1',{fire,{1,0}})),
    ?_assertEqual({TM2,nothing},tank_rules:apply_action(TM2,'T2',{fire,{10,10}})),
    ?_assertEqual({TM2,'T2',{fire,{1,1},false}},tank_rules:apply_action(TM2,'T2',{fire,{1,1}}))
    ].