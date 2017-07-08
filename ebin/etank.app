{application, etank,
 [{vsn, "1.0.0"},
  {description, "Tank fightiing it out in Erlang"},
  {modules, [tank_demo, tank_runner, tank_sup,
             tank_world,tank_rules,tank_board]},
  {applications, [stdlib, kernel]},
  {registered, [tank_sup,tank_world]}
 ]}.