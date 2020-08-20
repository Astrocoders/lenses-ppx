open Asttypes;
open Parsetree;

let annotationName = "lenses";
let getAttributeByName = (attributes: list(attribute), name) => {
  let filtered =
    attributes |> List.filter(({attr_name: {txt, _},_}) => txt == name);

  switch (filtered) {
  | [] => Ok(None)
  | [attribute] => Ok(Some(attribute))
  | _ => Error("Too many occurrences of \"" ++ name ++ "\" attribute")
  };
};

type generatorSettings = {lenses: bool};
let getSettingsFromAttributes = ( attributes: list(attribute) ) =>
  switch (getAttributeByName(attributes, annotationName)) {
  | Ok(Some(_)) => Ok(Some({lenses: true}))
  | Ok(None) => Ok(None)
  | Error(_) as e => e
  };


let fail = (loc, message) =>
    Location.error(~loc, message)
    |> (v) => Location.Error(v)
    |> raise;