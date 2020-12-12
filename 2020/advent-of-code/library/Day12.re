module Action = {
  type t =
    | N(int)
    | S(int)
    | E(int)
    | W(int)
    | L(int)
    | R(int)
    | F(int);

  let value =
    fun
    | N(v)
    | S(v)
    | E(v)
    | W(v)
    | L(v)
    | R(v)
    | F(v) => v;

  let of_string = s => {
    let dir = s |> String.sub(_, 0, 1);
    let len = s |> String.length;
    let value = s |> String.sub(_, 1, len - 1) |> int_of_string;
    switch (dir) {
    | "N" => N(value)
    | "S" => S(value)
    | "E" => E(value)
    | "W" => W(value)
    | "F" => F(value)
    | "L" => L(value)
    | "R" => R(value)
    | _ => failwith("invalid direction")
    };
  };
};

let path = "./bin/input/input_day_12";
let testPath = "./bin/input/input_day_12_test";

module StateMachine = {
  type direction =
    | North
    | South
    | East
    | West;

  type state = {
    direction,
    horizonal: int,
    vertical: int,
  };

  let manhattanDistance = s => abs(s.horizonal) + abs(s.vertical);

  let initial = () => {direction: East, horizonal: 0, vertical: 0};

  let moveForward = (state, action) => {
    let dir = state.direction;
    let distance = action |> Action.value;
    switch (dir) {
    | North => {...state, vertical: state.vertical + distance}
    | South => {...state, vertical: state.vertical - distance}
    | East => {...state, horizonal: state.horizonal + distance}
    | West => {...state, horizonal: state.horizonal - distance}
    };
  };

  let moveHorizontal = (state, action) => {
    let horizonal =
      switch (action) {
      | Action.E(distance) => state.horizonal + distance
      | Action.W(distance) => state.horizonal - distance
      | _ => failwith("invalid case")
      };
    {...state, horizonal};
  };

  let moveVertical = (state, action) => {
    let vertical =
      switch (action) {
      | Action.N(distance) => state.vertical + distance
      | Action.S(distance) => state.vertical - distance
      | _ => failwith("invalid case")
      };
    {...state, vertical};
  };

  let moveLeft = (state, action) => {
    let angle = action |> Action.value;
    let dir = state.direction;
    let rec aux = (dir, count) =>
      if (count == 0) {
        dir;
      } else {
        let dir =
          switch (dir) {
          | North => West
          | West => South
          | South => East
          | East => North
          };
        aux(dir, count - 1);
      };
    let direction = aux(dir, angle / 90);
    {...state, direction};
  };

  let moveRight = (state, action) => {
    let angle = action |> Action.value;
    let dir = state.direction;
    let rec aux = (dir, count) =>
      if (count == 0) {
        dir;
      } else {
        let dir =
          switch (dir) {
          | North => East
          | East => South
          | South => West
          | West => North
          };
        aux(dir, count - 1);
      };
    let direction = aux(dir, angle / 90);
    {...state, direction};
  };

  let next = (state, action) => {
    switch (action) {
    | Action.N(_)
    | Action.S(_) => moveVertical(state, action)
    | Action.E(_)
    | Action.W(_) => moveHorizontal(state, action)
    | Action.F(_) => moveForward(state, action)
    | Action.L(_) => moveLeft(state, action)
    | Action.R(_) => moveRight(state, action)
    };
  };
};

let run = () => {
  let initialState = StateMachine.initial();
  let finalState =
    Util.getLinesFromFile(path)
    |> List.rev
    |> List.map(Action.of_string)
    |> List.fold_left(StateMachine.next, initialState);
  let distance = finalState |> StateMachine.manhattanDistance;

  Console.log("Part 1> " ++ string_of_int(distance));
};