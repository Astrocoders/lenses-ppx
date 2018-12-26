module FormConfig = struct
  [%lenses

  type state = {
    email: string;
    age: int;
  };;
]
end;;

[%lenses

  type user = {
    email: string;
    age: int;
  };;
]
