module State = [%lenses
  type state = {
    email: string,
    age: int,
  }
];

let state: State.state = {email: "", age: 0};
