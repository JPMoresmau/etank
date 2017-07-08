# Erlang Tank Game

This is a little toy project in Erlang. You can program a tank and battle it out with others! See `tank_demo.erl` for some examples of very simple tanks and starting the battle.

## Principles

- To program a tank, you need to give it a name, an initial state and an update function that takes the current state and the current world (a `tank_map` record), and returns the new state and an action (the action can be either `move_north`,`move_east`, `move_south`, `move_west` or `fire` with the coordinates of the target).
- Each tank is then driven by a gen_server process (`tank_runner`)
- There is a supervisor (dynamic single_for_one `tank_sup`) coordinating all the tanks
- A gen_server (`tank_world`) is the top process, managing the supervisor and regularly asking the tanks for their next action
- `tank_rules` is the library that updates the world from actions, calculates the winner, etc.

Some functions in tank_rules are quite simple and could be made smarter (like `move_toward`).