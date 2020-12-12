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
    horizontal: int,
    vertical: int,
  };

  let manhattanDistance = s => abs(s.horizontal) + abs(s.vertical);

  let initial = (~horizontal=0, ~vertical=0, ()) => {
    direction: East,
    horizontal,
    vertical,
  };

  let moveForward = (state, action) => {
    let dir = state.direction;
    let distance = action |> Action.value;
    switch (dir) {
    | North => {...state, vertical: state.vertical + distance}
    | South => {...state, vertical: state.vertical - distance}
    | East => {...state, horizontal: state.horizontal + distance}
    | West => {...state, horizontal: state.horizontal - distance}
    };
  };

  let moveHorizontal = (state, action) => {
    let horizontal =
      switch (action) {
      | Action.E(distance) => state.horizontal + distance
      | Action.W(distance) => state.horizontal - distance
      | _ => failwith("invalid case")
      };
    {...state, horizontal};
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

module StateMachine2 = {
  type direction =
    | North
    | South
    | East
    | West;

  type position = {
    x: int,
    y: int,
  };

  type state = {
    ship: position,
    waypoint: position,
  };

  let initial = (~shipX, ~shipY, ~wayPointX, ~wayPointY) => {
    ship: {
      x: shipX,
      y: shipY,
    },
    waypoint: {
      x: wayPointX,
      y: wayPointY,
    },
  };

  let moveHorizontal = (state, action) => {
    let horizontal =
      switch (action) {
      | Action.E(distance) => state.waypoint.x + distance
      | Action.W(distance) => state.waypoint.x - distance
      | _ => failwith("invalid case")
      };
    {
      ...state,
      waypoint: {
        ...state.waypoint,
        x: horizontal,
      },
    };
  };

  let moveVertical = (state, action) => {
    let vertical =
      switch (action) {
      | Action.N(distance) => state.waypoint.y + distance
      | Action.S(distance) => state.waypoint.y - distance
      | _ => failwith("invalid case")
      };
    {
      ...state,
      waypoint: {
        ...state.waypoint,
        y: vertical,
      },
    };
  };

  let rotateRight = (state, action) => {
    let angle = action |> Action.value;
    let rec aux = (acc, count) =>
      if (count == 0) {
        acc;
      } else {
        let x = acc.x;
        let y = acc.y;
        let acc = {x: y, y: - x};
        aux(acc, count - 1);
      };
    let waypoint = aux(state.waypoint, angle / 90);
    {...state, waypoint};
  };

  let rotateLeft = (state, action) => {
    let angle = action |> Action.value;
    let rec aux = (acc, count) =>
      if (count == 0) {
        acc;
      } else {
        let x = acc.x;
        let y = acc.y;
        let acc = {x: - y, y: x};
        aux(acc, count - 1);
      };
    let waypoint = aux(state.waypoint, angle / 90);
    {...state, waypoint};
  };

  let moveForward = (state, action) => {
    let waypoint = state.waypoint;
    let ship = state.ship;
    let ship =
      switch (action) {
      | Action.F(n) => {
          x: ship.x + n * waypoint.x,
          y: ship.y + n * waypoint.y,
        }
      | _ => failwith("invalid case")
      };
    {...state, ship};
  };

  let next = (state, action) => {
    switch (action) {
    | Action.N(_)
    | Action.S(_) => moveVertical(state, action)
    | Action.E(_)
    | Action.W(_) => moveHorizontal(state, action)
    | Action.F(_) => moveForward(state, action)
    | Action.L(_) => rotateLeft(state, action)
    | Action.R(_) => rotateRight(state, action)
    };
  };

  let manhattanDistance = s => abs(s.ship.x) + abs(s.ship.y);
};

let run = () => {
  print_endline("---------- Day 12 ----------");
  let initialState = StateMachine.initial();
  let finalState =
    Util.getLinesFromFile(path)
    |> List.rev
    |> List.map(Action.of_string)
    |> List.fold_left(StateMachine.next, initialState);
  let distance = finalState |> StateMachine.manhattanDistance;

  Console.log("Part 1> " ++ string_of_int(distance));

  let initialState2 =
    StateMachine2.initial(~shipX=0, ~shipY=0, ~wayPointX=10, ~wayPointY=1);
  let finalState =
    Util.getLinesFromFile(path)
    |> List.rev
    |> List.map(Action.of_string)
    |> List.fold_left(StateMachine2.next, initialState2);

  let distance = finalState |> StateMachine2.manhattanDistance;
  Console.log("Part 2> " ++ string_of_int(distance));
};