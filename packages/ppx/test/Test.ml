module FormConfig = struct
  module State = [%lenses
  type t = {
    email: string;
    age: int;
  };;
]
end;;

module User = [%lenses
  type t = {
    email: string;
    age: int;
  };;
]

module OnlyOneField = [%lenses
  type t = {
    email: string;
  };;
];;

type profile = {
  name: string;
  isActive: int;
}
[@@lenses]