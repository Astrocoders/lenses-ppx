module StateLenses = [%lenses
 type state = {
   email: string,
   age: int,
 }
];


open StateLenses;

let state = {email: "fakenickels@brazil.gov.br", age: 0};

Js.log(state->get(Email));
Js.log(state->get(Age));
