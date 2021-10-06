# lenses-ppx

## What are GADTs?
[GADTs: A primer](https://sketch.sh/s/yH0MJiujNSiofDWOU85loX/)

# Why
Differently from normal lenses/optics the following approach allows for composing "lenses" into lists/arrays which is them useful for things like https://github.com/rescriptbr/reschema

# Install
Install the last stable version

For ReScript
```
npm install --save-dev lenses-ppx@latest
or
yarn add lenses-ppx@latest -D
```

For BuckleScript < 6
```
npm install --save-dev lenses-ppx@4.0.0
or
yarn add lenses-ppx@4.0.0 -D
```

# Build
```
npm run build
```

# Watch

```
npm run watch
```

In
```reason
module StateLenses = [%lenses
 type state = {
   email: string,
   age: int,
 }
]
```

Out

```reason
module StateLenses = {
  type state = {
    email: string,
    age: int,
  };
  type field(_) =
    | Email: field(string)
    | Age: field(int);
  let get: type value. (state, field(value)) => value =
    (state, field) =>
      switch (field) {
      | Email => state.email
      | Age => state.age
      };
  let set: type value. (state, field(value), value) => state =
    (state, field, value) =>
      switch (field) {
      | Email => {...state, email: value}
      | Age => {...state, age: value}
      };
};
```
Using
```reason
open StateLenses;

let state = {email: "fakenickels@gov.br", age: 969};

Js.log(state->get(Email));
Js.log(state->get(Age));
```


Alternatively you can also use it like
```reason
[@lenses]
[@decco]
type bartux = {
  color: string,
  top: int,
};

let bartux = {color: "red", top: 10};

Js.log(bartux->bartux_get(Color));
Js.log(bartux->bartux_set(Top, 20));
Js.log(bartux_encode(bartux));
```


Alternatives

- https://github.com/scoville/re-optic/blob/master/docs/lenses-ppx.md which is more strict and follows more closely Optics standards
