module FormConfig = struct
  module State = [%lenses
    type t = {
      email: string;
      age: int;
      hobbies: string array;
    }
  ]
end

module User = [%lenses
  type t = {
    email: string;
    age: int;
    hobbies: string array;
  }
]
