module State = [%lenses
  type state = {
    email: string,
    age: int,
    hobbies: array(string),
  }
];

let state: State.state = {email: "", age: 0, hobbies: [|"foo", "bar"|]};
