open Migrate_parsetree;
open OCaml_406.Ast;

open Ast_mapper;
open Asttypes;
open Parsetree;
open Ast_helper;

let gadtFieldName = "field";
let loc = Location.none;

let createSetLens = (~typeName, ~fields) => {
  pstr_loc: Location.none,
  pstr_desc:
    Pstr_value(
      Nonrecursive,
      [
        {
          pvb_loc: loc,
          pvb_attributes: [],
          pvb_pat: {
            ppat_loc: Location.none,
            ppat_attributes: [],
            ppat_desc:
              Ppat_constraint(
                {
                  ppat_desc: Ppat_var({txt: "set", loc}),
                  ppat_loc: Location.none,
                  ppat_attributes: [],
                },
                {
                  ptyp_loc: Location.none,
                  ptyp_attributes: [],
                  ptyp_desc:
                    Ptyp_poly(
                      [{txt: "value", loc}],
                      {
                        ptyp_loc: Location.none,
                        ptyp_attributes: [],
                        ptyp_desc:
                          Ptyp_arrow(
                            Nolabel,
                            {
                              ptyp_desc:
                                Ptyp_constr(
                                  {txt: Lident(typeName), loc},
                                  [],
                                ),
                              ptyp_loc: Location.none,
                              ptyp_attributes: [],
                            },
                            {
                              ptyp_loc: Location.none,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_arrow(
                                  Nolabel,
                                  {
                                    ptyp_loc: Location.none,
                                    ptyp_attributes: [],
                                    ptyp_desc:
                                      Ptyp_constr(
                                        {txt: Lident(gadtFieldName), loc},
                                        [
                                          {
                                            ptyp_desc: Ptyp_var("value"),
                                            ptyp_loc: Location.none,
                                            ptyp_attributes: [],
                                          },
                                        ],
                                      ),
                                  },
                                  {
                                    ptyp_loc: Location.none,
                                    ptyp_attributes: [],
                                    ptyp_desc:
                                      Ptyp_arrow(
                                        Nolabel,
                                        {
                                          ptyp_desc: Ptyp_var("value"),
                                          ptyp_loc: Location.none,
                                          ptyp_attributes: [],
                                        },
                                        {
                                          ptyp_loc: Location.none,
                                          ptyp_attributes: [],
                                          ptyp_desc:
                                            Ptyp_constr(
                                              {txt: Lident(typeName), loc},
                                              [],
                                            ),
                                        },
                                      ),
                                  },
                                ),
                            },
                          ),
                      },
                    ),
                },
              ),
          },
          pvb_expr: {
            pexp_loc: loc,
            pexp_attributes: [],
            pexp_desc:
              Pexp_newtype(
                { txt: "value", loc },
                {
                  pexp_loc: loc,
                  pexp_attributes: [],
                  pexp_desc:
                    Pexp_constraint(
                      {
                        pexp_loc: loc,
                        pexp_attributes: [],
                        pexp_desc:
                          Pexp_fun(
                            Nolabel,
                            None,
                            {
                              ppat_desc: Ppat_var({txt: typeName, loc}),
                              ppat_loc: Location.none,
                              ppat_attributes: [],
                            },
                            {
                              pexp_loc: loc,
                              pexp_attributes: [],
                              pexp_desc:
                                Pexp_fun(
                                  Nolabel,
                                  None,
                                  {
                                    ppat_loc: Location.none,
                                    ppat_attributes: [],
                                    ppat_desc:
                                      Ppat_var({txt: gadtFieldName, loc}),
                                  },
                                  {
                                    pexp_loc: loc,
                                    pexp_attributes: [],
                                    pexp_desc:
                                      Pexp_fun(
                                        Nolabel,
                                        None,
                                        {
                                          ppat_loc: Location.none,
                                          ppat_attributes: [],
                                          ppat_desc:
                                            Ppat_var({txt: "value", loc}),
                                        },
                                        {
                                          pexp_loc: loc,
                                          pexp_attributes: [],
                                          pexp_desc:
                                            Pexp_match(
                                              {
                                                pexp_loc: loc,
                                                pexp_attributes: [],
                                                pexp_desc:
                                                  Pexp_ident({
                                                    txt:
                                                      Lident(gadtFieldName),
                                                    loc,
                                                  }),
                                              },
                                              List.map(
                                                field =>
                                                  {
                                                    pc_lhs: {
                                                      ppat_loc: Location.none,
                                                      ppat_attributes: [],
                                                      ppat_desc:
                                                        Ppat_construct(
                                                          {
                                                            loc,
                                                            txt:
                                                              Lident(
                                                                String.capitalize(
                                                                  field.
                                                                    pld_name.
                                                                    txt,
                                                                ),
                                                              ),
                                                          },
                                                          None,
                                                        ),
                                                    },
                                                    pc_guard: None,
                                                    pc_rhs: {
                                                      pexp_loc: loc,
                                                      pexp_attributes: [],
                                                      pexp_desc:
                                                        Pexp_record(
                                                          [
                                                            (
                                                              {
                                                                loc,
                                                                txt:
                                                                  Lident(
                                                                    field.
                                                                    pld_name.
                                                                    txt,
                                                                  ),
                                                              },
                                                              {
                                                                pexp_loc: loc,
                                                                pexp_attributes:
                                                                  [],
                                                                pexp_desc:
                                                                  Pexp_ident({
                                                                    loc,
                                                                    txt:
                                                                    Lident(
                                                                    "value",
                                                                    ),
                                                                  }),
                                                              },
                                                            ),
                                                          ],
                                                          Some({
                                                            pexp_loc: loc,
                                                            pexp_attributes:
                                                              [],
                                                            pexp_desc:
                                                              Pexp_ident({
                                                                loc,
                                                                txt:
                                                                  Lident(
                                                                    typeName,
                                                                  ),
                                                              }),
                                                          }),
                                                        ),
                                                    },
                                                  },
                                                fields,
                                              ),
                                            ),
                                        },
                                      ),
                                  },
                                ),
                            },
                          ),
                      },
                      {
                        ptyp_loc: loc,
                        ptyp_attributes: [],
                        ptyp_desc:
                          Ptyp_arrow(
                            Nolabel,
                            {
                              ptyp_loc: loc,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_constr(
                                  {txt: Lident(typeName), loc},
                                  [],
                                ),
                            },
                            {
                              ptyp_loc: loc,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_arrow(
                                  Nolabel,
                                  {
                                    ptyp_loc: loc,
                                    ptyp_attributes: [],
                                    ptyp_desc:
                                      Ptyp_constr(
                                        {txt: Lident(gadtFieldName), loc},
                                        [
                                          {
                                            ptyp_loc: loc,
                                            ptyp_attributes: [],
                                            ptyp_desc:
                                              Ptyp_constr(
                                                {txt: Lident("value"), loc},
                                                [],
                                              ),
                                          },
                                        ],
                                      ),
                                  },
                                  {
                                    ptyp_loc: loc,
                                    ptyp_attributes: [],
                                    ptyp_desc:
                                      Ptyp_arrow(
                                        Nolabel,
                                        {
                                          ptyp_loc: loc,
                                          ptyp_attributes: [],
                                          ptyp_desc:
                                            Ptyp_constr(
                                              {txt: Lident("value"), loc},
                                              [],
                                            ),
                                        },
                                        {
                                          ptyp_loc: loc,
                                          ptyp_attributes: [],
                                          ptyp_desc:
                                            Ptyp_constr(
                                              {txt: Lident(typeName), loc},
                                              [],
                                            ),
                                        },
                                      ),
                                  },
                                ),
                            },
                          ),
                      },
                    ),
                },
              ),
          },
        },
      ],
    ),
};

let createGetLens = (~typeName, ~fields) => {
  pstr_loc: Location.none,
  pstr_desc:
    Pstr_value(
      Nonrecursive,
      [
        {
          pvb_loc: loc,
          pvb_attributes: [],
          pvb_pat: {
            ppat_loc: Location.none,
            ppat_attributes: [],
            ppat_desc:
              Ppat_constraint(
                {
                  ppat_desc: Ppat_var({txt: "get", loc: Location.none}),
                  ppat_loc: Location.none,
                  ppat_attributes: [],
                },
                {
                  ptyp_loc: Location.none,
                  ptyp_attributes: [],
                  ptyp_desc:
                    Ptyp_poly(
                      [{txt: "value", loc}],
                      {
                        ptyp_loc: Location.none,
                        ptyp_attributes: [],
                        ptyp_desc:
                          Ptyp_arrow(
                            Nolabel,
                            {
                              ptyp_loc: Location.none,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_constr(
                                  {
                                    txt: Lident(typeName),
                                    loc: Location.none,
                                  },
                                  [],
                                ),
                            },
                            {
                              ptyp_loc: Location.none,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_arrow(
                                  Nolabel,
                                  {
                                    ptyp_loc: Location.none,
                                    ptyp_attributes: [],
                                    ptyp_desc:
                                      Ptyp_constr(
                                        {
                                          txt: Lident(gadtFieldName),
                                          loc: Location.none,
                                        },
                                        [
                                          {
                                            ptyp_desc: Ptyp_var("value"),
                                            ptyp_loc: Location.none,
                                            ptyp_attributes: [],
                                          },
                                        ],
                                      ),
                                  },
                                  {
                                    ptyp_desc: Ptyp_var("value"),
                                    ptyp_loc: Location.none,
                                    ptyp_attributes: [],
                                  },
                                ),
                            },
                          ),
                      },
                    ),
                },
              ),
          },
          pvb_expr: {
            pexp_loc: loc,
            pexp_attributes: [],
            pexp_desc:
              Pexp_newtype(
                { txt: "value", loc },
                {
                  pexp_loc: loc,
                  pexp_attributes: [],
                  pexp_desc:
                    Pexp_constraint(
                      {
                        pexp_loc: loc,
                        pexp_attributes: [],
                        pexp_desc:
                          Pexp_fun(
                            Nolabel,
                            None,
                            {
                              ppat_desc:
                                Ppat_var({txt: typeName, loc: Location.none}),
                              ppat_loc: Location.none,
                              ppat_attributes: [],
                            },
                            {
                              pexp_loc: loc,
                              pexp_attributes: [],
                              pexp_desc:
                                Pexp_fun(
                                  Nolabel,
                                  None,
                                  {
                                    ppat_desc:
                                      Ppat_var({txt: gadtFieldName, loc}),
                                    ppat_loc: loc,
                                    ppat_attributes: [],
                                  },
                                  {
                                    pexp_loc: loc,
                                    pexp_attributes: [],
                                    pexp_desc:
                                      Pexp_match(
                                        {
                                          pexp_loc: loc,
                                          pexp_attributes: [],
                                          pexp_desc:
                                            Pexp_ident({
                                              txt: Lident(gadtFieldName),
                                              loc,
                                            }),
                                        },
                                        List.map(
                                          field =>
                                            {
                                              pc_lhs: {
                                                ppat_loc: loc,
                                                ppat_attributes: [],
                                                ppat_desc:
                                                  Ppat_construct(
                                                    {
                                                      loc,
                                                      txt:
                                                        Lident(
                                                          String.capitalize(
                                                            field.pld_name.txt,
                                                          ),
                                                        ),
                                                    },
                                                    None,
                                                  ),
                                              },
                                              pc_guard: None,
                                              pc_rhs: {
                                                pexp_loc: loc,
                                                pexp_attributes: [],
                                                pexp_desc:
                                                  Pexp_field(
                                                    {
                                                      pexp_loc: loc,
                                                      pexp_attributes: [],
                                                      pexp_desc:
                                                        Pexp_ident({
                                                          loc,
                                                          txt:
                                                            Lident(typeName),
                                                        }),
                                                    },
                                                    {
                                                      loc,
                                                      txt:
                                                        Lident(
                                                          field.pld_name.txt,
                                                        ),
                                                    },
                                                  ),
                                              },
                                            },
                                          fields,
                                        ),
                                      ),
                                  },
                                ),
                            },
                          ),
                      },
                      {
                        ptyp_loc: loc,
                        ptyp_attributes: [],
                        ptyp_desc:
                          Ptyp_arrow(
                            Nolabel,
                            {
                              ptyp_loc: loc,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_constr(
                                  {txt: Lident(typeName), loc},
                                  [],
                                ),
                            },
                            {
                              ptyp_loc: loc,
                              ptyp_attributes: [],
                              ptyp_desc:
                                Ptyp_arrow(
                                  Nolabel,
                                  {
                                    ptyp_loc: loc,
                                    ptyp_attributes: [],
                                    ptyp_desc:
                                      Ptyp_constr(
                                        {txt: Lident(gadtFieldName), loc},
                                        [
                                          {
                                            ptyp_loc: loc,
                                            ptyp_attributes: [],
                                            ptyp_desc:
                                              Ptyp_constr(
                                                {txt: Lident("value"), loc},
                                                [],
                                              ),
                                          },
                                        ],
                                      ),
                                  },
                                  {
                                    ptyp_loc: loc,
                                    ptyp_attributes: [],
                                    ptyp_desc:
                                      Ptyp_constr(
                                        {txt: Lident("value"), loc},
                                        [],
                                      ),
                                  },
                                ),
                            },
                          ),
                      },
                    ),
                },
              ),
          },
        },
      ],
    ),
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
                      txt: String.capitalize(field.pld_name.txt),
                      loc: Location.none,
                    },
                    pcd_args: Pcstr_tuple([]),
                    pcd_res:
                      Some({
                        ptyp_loc: Location.none,
                        ptyp_attributes: [],
                        ptyp_desc:
                          Ptyp_constr(
                            {txt: Lident(gadtFieldName), loc: Location.none},
                            [
                              {
                                ptyp_desc: field.pld_type.ptyp_desc,
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
          pstr_loc: Location.none,
          pstr_desc:
            Pstr_type(
              rec_flag,
              [
                {
                  ptype_name: {
                    txt: typeName,
                    loc: Location.none,
                  },
                  ptype_kind: Ptype_record(fields),
                  ptype_params: [],
                  ptype_cstrs: [],
                  ptype_private: Public,
                  ptype_manifest: None,
                  ptype_attributes: [],
                  ptype_loc: Location.none,
                },
              ],
            ),
        },
        ~typeName,
        ~fields,
      )
    | _ => default_mapper.module_expr(mapper, expr)
    },
};

let () =
  Driver.register(
    ~name="lenses-ppx",
    ~args=[],
    (module OCaml_406),
    lensesMapper,
  );
