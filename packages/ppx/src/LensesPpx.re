open Migrate_parsetree;
open Ast_410;
open Ast_mapper;
open Asttypes;
open Parsetree;
open Ast_helper;

let gadtFieldName = "field";
let loc = Location.none;
let createSetLens = (~typeName, ~fields) => {
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

  let typeDefinition =
    Ast_helper.Typ.poly(
      [{txt: "value", loc}],
      [%type: ([%t recordType], field('value), 'value) => [%t recordType]],
    )
    |> Ast_helper.Typ.force_poly;

  let typeDefinitionFilledWithPolyLocalType = [%type:
    ([%t recordType], field(value), value) => [%t recordType]
  ];

  let patMatch =
    Ast_helper.Exp.mk(
      Pexp_match(
        Ast_helper.Exp.mk(Pexp_ident({txt: Lident(gadtFieldName), loc})),
        cases,
      ),
    );

  // Properly applying type constraints for the poly local abstract type
  // https://caml.inria.fr/pub/docs/manual-ocaml/locallyabstract.html#p:polymorpic-locally-abstract
  let body = [%expr (values, field, value) => [%e patMatch]];
  let pat = Ast_helper.Pat.constraint_([%pat? set], typeDefinition);
  let body =
    Ast_helper.Exp.constraint_(body, typeDefinitionFilledWithPolyLocalType);

  [%stri let [%p pat] = (type value) => [%e body]];
};

let createGetLens = (~typeName, ~fields) => {
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

  let typeDefinition =
    Ast_helper.Typ.poly(
      [{txt: "value", loc}],
      [%type: ([%t recordType], field('value)) => 'value],
    )
    |> Ast_helper.Typ.force_poly;

  let typeDefinitionFilledWithPolyLocalType = [%type:
    ([%t recordType], field(value)) => value
  ];

  let patMatch =
    Ast_helper.Exp.mk(
      Pexp_match(
        Ast_helper.Exp.mk(Pexp_ident({txt: Lident(gadtFieldName), loc})),
        cases,
      ),
    );

  // Properly applying type constraints for the poly local abstract type
  // https://caml.inria.fr/pub/docs/manual-ocaml/locallyabstract.html#p:polymorpic-locally-abstract
  let body = [%expr (values, field) => [%e patMatch]];
  let pat = Ast_helper.Pat.constraint_([%pat? get], typeDefinition);
  let body =
    Ast_helper.Exp.constraint_(body, typeDefinitionFilledWithPolyLocalType);

  [%stri let [%p pat] = (type value) => [%e body]];
};

let createGadt = (~fields) => {
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
              Invariant,
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

let createModule = (~typeDef, ~typeName, ~fields) =>
  Mod.mk(
    Pmod_structure([
      typeDef,
      createGadt(~fields),
      createGetLens(~typeName, ~fields),
      createSetLens(~typeName, ~fields),
    ]),
  );

let lensesMapper = (_, _) => {
  ...default_mapper,
  module_expr: (mapper, expr) =>
    switch (expr) {
    | {
        pmod_desc:
          Pmod_extension((
            {txt: "lenses"},
            PStr([
              {
                pstr_desc:
                  Pstr_type(
                    rec_flag,
                    [
                      {
                        ptype_name: {txt: typeName},
                        ptype_kind: Ptype_record(fields),
                      },
                    ],
                  ),
              },
            ]),
          )),
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
    | _ => default_mapper.module_expr(mapper, expr)
    },
};

let () =
  Driver.register(~name="lenses-ppx", Versions.ocaml_410, lensesMapper);
