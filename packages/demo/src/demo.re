module StateLenses = [%lenses
  type state = {
    email: string,
    age: int,
    hobbies: array(string),
  }
];
open StateLenses;

let state = {
  email: "fakenickels@brazil.gov.br",
  age: 0,
  hobbies: [|"foo", "bar"|],
};

Js.log(state->get(Email));
Js.log(state->get(Age));
Js.log(state->get(HobbiesAt(1)));
