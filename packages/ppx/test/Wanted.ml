module FormConfig =
  struct
    module State =
      struct
        type t = {
          email: string;
          age: int;
          hobbies: string array;}
        type _ field =
          | Email: string field
          | Age: int field
          | Hobbies: string array field
          | HobbiesAt: int -> string option field
          | HobbiesAtExn: int -> string field
        let get : 'value . t -> 'value field -> 'value= fun (type value) ->
          (fun t  ->
             fun field  ->
               match field with
               | Email  -> t.email
               | Age  -> t.age
               | Hobbies  -> t.hobbies
               | HobbiesAt index ->
                   (try Some ((t.hobbies).(index)) with | _ -> None)
               | HobbiesAtExn index -> (t.hobbies).(index) : t ->
                                                               value field ->
                                                                 value)
        let set : 'value . t -> 'value field -> 'value -> t= fun (type value)
          ->
          (fun t  ->
             fun field  ->
               fun value  ->
                 match field with
                 | Email  -> { t with email = value }
                 | Age  -> { t with age = value }
                 | Hobbies  -> { t with hobbies = value }
                 | HobbiesAt index ->
                     (match value with
                      | Some value ->
                          ((t.hobbies).(index) <- value;
                           { t with hobbies = (t.hobbies) })
                      | None  -> t)
                 | HobbiesAtExn index ->
                     ((t.hobbies).(index) <- value;
                      { t with hobbies = (t.hobbies) }) : t ->
                                                            value field ->
                                                              value -> t)
      end
  end
