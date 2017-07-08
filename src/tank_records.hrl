%%% Records used in several modules
%% The Tank item used in runner
-record(tank,{
    name,
    state,
    update_function
    }). 

%% A Tank and its reference
-record(tank_ref,{
    atom_name,
    ref,
    tank
    }).

%% Tank information in world
-record(tank_info,{
    atom_name,
    index=1,
    health=10,
    pos
    }).

%% Map of tanks
-record(tank_map,{
    size={10,10},
    tanks=gb_trees:empty()
    }).
