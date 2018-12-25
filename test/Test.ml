module FormConfig = struct
  [%lenses]
  type state = {
    email: string;
    age: int;
  };;
end
