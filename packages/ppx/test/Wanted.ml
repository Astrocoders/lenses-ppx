module User =
  struct
    type t = {
      email: string;
      age: int;
      hobbies: string array;}
    type _ field =
      | Email: string field
      | Age: int field
      | Hobbies: int -> string option field
    let get : 'value . t -> 'value field -> 'value= fun (type value) ->
      (fun t  ->
         fun field  ->
           match field with
           | Email  -> t.email
           | Age  -> t.age
           | Hobbies index ->
               (try Some ((t.hobbies).(index)) with | _ -> None) : t ->
                                                                    value
                                                                    field ->
                                                                    value)
    let set : 'value . t -> 'value field -> 'value -> t= fun (type value) ->
      (fun t  ->
         fun field  ->
           fun value  ->
             match field with
             | Email  -> { t with email = value }
             | Age  -> { t with age = value }
             | Hobbies index ->
                 (match value with
                  | Some value ->
                      ((t.hobbies).(index) <- value;
                       { t with hobbies = (t.hobbies) })
                  | None  -> hobbies) : t -> value field -> value -> t)
end
