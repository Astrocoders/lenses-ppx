module FormConfig =
  struct
    type _ field =
      | Email: string field
      | Age: int field
    type state = {
      email: string;
      age: int;}
    let get : 'value . state -> 'value field -> 'value= fun (type value) ->
      (fun state  ->
         fun field  ->
           match field with | Email  -> state.email | Age  -> state.age : 
      state -> value field -> value)
    let set : 'value . state -> 'value field -> 'value -> state= fun (type
      value) ->
      (fun state  ->
         fun field  ->
           fun value  ->
             match field with
             | Email  -> { state with email = value }
             | Age  -> { state with age = value } : state ->
                                                      value field ->
                                                        value -> state)
  end
