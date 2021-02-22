module StateLenses = %lenses(
  type state = {
    email: string,
    age: int,
  }
)

open StateLenses

let state = {email: "fakenickels@brazil.gov.br", age: 0}

Js.log(state->get(Email))
Js.log(state->get(Age))

@lenses @decco
type bartux = {
  color: string,
  top: int,
}

@deriving(abstract)
type should_not_conflict_with_deriving_abstract = {
  buy: string,
  bitcoin: string
};

let bartux = {color: "red", top: 10}

Js.log(bartux->bartux_get(Color))
Js.log(bartux->bartux_set(Top, 20))
Js.log(bartux_encode(bartux))
