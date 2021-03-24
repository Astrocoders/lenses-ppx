open Ppxlib;
open Asttypes;
open Parsetree;
open Ast_helper;

let loc = Location.none;
let createSetLens = (~typeName, ~gadtFieldName, ~prefix="", ~fields, ()) => {
  let cases =
    List.map(
      field => {
        Ast_helper.Exp.case(
          Ast_helper.Pat.construct(
            {loc, txt: Lident(String.capitalize_ascii(field.pld_name.txt))},
            None,
          ),
          Ast_helper.Exp.record(
            [({loc, txt: Lident(field.pld_name.txt)}, [%expr value])],
            // Spread not needed when there is only one field in the type.
            // So we avoid the "redundant with" warning
            List.length(fields) > 1 ? Some([%expr values]) : None,
          ),
        )
      },
      fields,
    );

  let recordType =
    Ast_helper.Typ.mk(Ptyp_constr({txt: Lident(typeName), loc}, []));

  let gadtTypePoly =
    Ast_helper.Typ.mk(
      Ptyp_constr({txt: Lident(gadtFieldName), loc}, [[%type: 'value]]),
    );

  let gadtTypeLocal =
    Ast_helper.Typ.mk(
      Ptyp_constr({txt: Lident(gadtFieldName), loc}, [[%type: value]]),
    );

  let typeDefinition =
    Ast_helper.Typ.poly(
      [{txt: "value", loc}],
      [%type: ([%t recordType], [%t gadtTypePoly], 'value) => [%t recordType]],
    )
    |> Ast_helper.Typ.force_poly;

  let typeDefinitionFilledWithPolyLocalType = [%type:
    ([%t recordType], [%t gadtTypeLocal], value) => [%t recordType]
  ];

  let patMatch =
    Ast_helper.Exp.mk(
      Pexp_match(
        Ast_helper.Exp.mk(Pexp_ident({txt: Lident("field"), loc})),
        cases,
      ),
    );

  // Properly applying type constraints for the poly local abstract type
  // https://caml.inria.fr/pub/docs/manual-ocaml/locallyabstract.html#p:polymorpic-locally-abstract
  let body = [%expr (values, field, value) => [%e patMatch]];
  let fnName = Ast_helper.Pat.var({txt: prefix ++ "set", loc});
  let pat = Ast_helper.Pat.constraint_(fnName, typeDefinition);
  let body =
    Ast_helper.Exp.constraint_(body, typeDefinitionFilledWithPolyLocalType);

  [%stri let [%p pat] = (type value) => [%e body]];
};

let createGetLens = (~typeName, ~gadtFieldName, ~prefix="", ~fields, ()) => {
  let cases =
    List.map(
      field => {
        Ast_helper.Exp.case(
          Ast_helper.Pat.construct(
            {loc, txt: Lident(String.capitalize_ascii(field.pld_name.txt))},
            None,
          ),
          Ast_helper.Exp.field(
            [%expr values],
            {loc, txt: Lident(field.pld_name.txt)},
          ),
        )
      },
      fields,
    );

  let recordType =
    Ast_helper.Typ.mk(Ptyp_constr({txt: Lident(typeName), loc}, []));

  let gadtTypePoly =
    Ast_helper.Typ.mk(
      Ptyp_constr({txt: Lident(gadtFieldName), loc}, [[%type: 'value]]),
    );

  let gadtTypeLocal =
    Ast_helper.Typ.mk(
      Ptyp_constr({txt: Lident(gadtFieldName), loc}, [[%type: value]]),
    );

  let typeDefinition =
    Ast_helper.Typ.poly(
      [{txt: "value", loc}],
      [%type: ([%t recordType], [%t gadtTypePoly]) => 'value],
    )
    |> Ast_helper.Typ.force_poly;

  let typeDefinitionFilledWithPolyLocalType = [%type:
    ([%t recordType], [%t gadtTypeLocal]) => value
  ];

  let patMatch =
    Ast_helper.Exp.mk(
      Pexp_match(
        Ast_helper.Exp.mk(Pexp_ident({txt: Lident("field"), loc})),
        cases,
      ),
    );

  // Properly applying type constraints for the poly local abstract type
  // https://caml.inria.fr/pub/docs/manual-ocaml/locallyabstract.html#p:polymorpic-locally-abstract
  let body = [%expr (values, field) => [%e patMatch]];
  let fnName = Ast_helper.Pat.var({txt: prefix ++ "get", loc});

  let pat = Ast_helper.Pat.constraint_(fnName, typeDefinition);
  let body =
    Ast_helper.Exp.constraint_(body, typeDefinitionFilledWithPolyLocalType);

  [%stri let [%p pat] = (type value) => [%e body]];
};

let createGadt = (~gadtFieldName, ~fields) => {
  pstr_loc: Location.none,
  pstr_desc:
    Pstr_type(
      Recursive,
      [
        {
          ptype_loc: Location.none,
          ptype_attributes: [],
          ptype_name: {
            txt: gadtFieldName,
            loc: Location.none,
          },
          ptype_params: [
            (
              {
                ptyp_loc_stack: [],
                ptyp_desc: Ptyp_any,
                ptyp_loc: Location.none,
                ptyp_attributes: [],
              },
              (NoVariance, NoInjectivity),
            ),
          ],
          ptype_cstrs: [],
          ptype_kind:
            Ptype_variant(
              List.map(
                field =>
                  {
                    pcd_loc: Location.none,
                    pcd_attributes: [],
                    pcd_name: {
                      txt: String.capitalize_ascii(field.pld_name.txt),
                      loc: Location.none,
                    },
                    pcd_args: Pcstr_tuple([]),
                    pcd_res:
                      Some({
                        ptyp_loc_stack: [],
                        ptyp_loc: Location.none,
                        ptyp_attributes: [],
                        ptyp_desc:
                          Ptyp_constr(
                            {txt: Lident(gadtFieldName), loc: Location.none},
                            [
                              {
                                ptyp_desc: field.pld_type.ptyp_desc,
                                ptyp_loc_stack: [],
                                ptyp_loc: Location.none,
                                ptyp_attributes: [],
                              },
                            ],
                          ),
                      }),
                  },
                fields,
              ),
            ),
          ptype_private: Public,
          ptype_manifest: None,
        },
      ],
    ),
};

let createStructureLenses =
    (~typeName, ~gadtFieldName, ~prefix=?, ~fields, ()) => {
  [
    createGadt(~gadtFieldName, ~fields),
    createGetLens(~typeName, ~gadtFieldName, ~prefix?, ~fields, ()),
    createSetLens(~typeName, ~gadtFieldName, ~prefix?, ~fields, ()),
  ];
};

let createModule = (~typeDef, ~typeName, ~fields) =>
  Mod.mk(
    Pmod_structure([
      typeDef,
      ...createStructureLenses(
           ~typeName,
           ~gadtFieldName="field",
           ~fields,
           (),
         ),
    ]),
  );

// Heavily borrowed from Decco's code
module StructureMapper = {
  open Utils;
  let mapTypeDecl = decl => {
    let {
      ptype_attributes,
      ptype_name: {txt: typeName, _},
      ptype_manifest,
      ptype_loc,
      ptype_kind,
      _,
    } = decl;

    switch (getSettingsFromAttributes(ptype_attributes)) {
    | Ok(Some({lenses: true})) =>
      switch (ptype_manifest, ptype_kind) {
      | (None, Ptype_abstract) =>
        fail(ptype_loc, "Can't generate lenses for unspecified type")
      | (None, Ptype_record(fields)) =>
        createStructureLenses(
          ~typeName,
          ~gadtFieldName=typeName ++ "_" ++ "field",
          ~prefix=typeName ++ "_",
          ~fields,
          (),
        )
      | _ => fail(ptype_loc, "This type is not handled by lenses-ppx")
      }
    | Ok(Some({lenses: false}))
    | Ok(None) => []
    | Error(s) => fail(ptype_loc, s)
    };
  };
  let mapStructureItem = (mapper, {pstr_desc, _} as structureItem) =>
    switch (pstr_desc) {
    | Pstr_type(_recFlag, decls) =>
      let valueBindings = decls |> List.map(mapTypeDecl) |> List.concat;
      [mapper#structure_item(structureItem)]
      @ (List.length(valueBindings) > 0 ? valueBindings : []);

    | _ => [mapper#structure_item(structureItem)]
    };
  let mapStructure = (mapper, structure) =>
    structure |> List.map(mapStructureItem(mapper)) |> List.concat;
};


class lensesMapper = {
  as self;
  inherit class Ast_traverse.map as super;

  pub! structure = structure => {
    StructureMapper.mapStructure(self, structure);
  };

  pub! module_expr = expr => {
    switch (expr) {
    | {
        pmod_desc:
          Pmod_extension((
            {txt: "lenses", _},
            PStr([
              {
                pstr_desc:
                  Pstr_type(
                    rec_flag,
                    [
                      {
                        ptype_name: {txt: typeName, _},
                        ptype_kind: Ptype_record(fields),
                        _,
                      },
                    ],
                  ),
                _,
              },
            ]),
          )),
        _,
      } =>
      createModule(
        ~typeDef={
          Ast_helper.Str.type_(
            rec_flag,
            [
              Ast_helper.Type.mk(
                ~kind=Ptype_record(fields),
                {txt: typeName, loc: Location.none},
              ),
            ],
          );
        },
        ~typeName,
        ~fields,
      )
    | _ => super#module_expr(expr)
    };
  };
};

let structure_mapper = s => (new lensesMapper)#structure(s);

let () =
  Driver.register_transformation(
    ~preprocess_impl=structure_mapper,
    "lenses-ppx"
  );

