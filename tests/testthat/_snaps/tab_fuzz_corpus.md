# corpus fixtures abort with their named messages

    Code
      writeLines(paste0(fixtures, ": ", msgs))
    Output
      bound_dup.tab: x Duplicate lower bound in declaration: "Coefficient (ge 0, ge 1) (all,r,REG) BNDP(r) # duplicate lower bound #" i One lower (`ge`/`gt`) and one upper (`le`/`lt`) bound are allowed per declaration (GEMPACK manual 10.19.1).
      default_eq_levels.tab: x Equation `(default=levels)` is not supported; the solver handles linearized equations only (GEMPACK manual 10.19): "Equation (default=levels)"
      default_positional.tab: x Default statements are not supported by the teems pipeline: "Coefficient (default=parameter)" i Declare the qualifier on each affected statement instead; the positional Default semantics (GEMPACK manual 10.19) cannot be carried through model preparation.
      formula_and_equation.tab: x Malformed `Formula & Equation` statement: expected `Formula [(initial)] & Equation [(levels)] name [quantifiers] lhs = rhs` (GEMPACK manual 10.9.1): "Formula & Equation E_ppl # malformed: no equals # (all,r,REG) PPL(r)"
      formula_no_equals.tab: x Formula statement without `=`: "Formula NOEQ 1" i Either the statement is malformed or its leading token is an unrecognized keyword that was read as an implicit Formula continuation.
      mapping_malformed.tab: x Malformed Mapping statement: "Mapping REGTOBLOC of REG onto BLOC" i Expected `Mapping [(onto)] <name> from <set> to <set>;` (GEMPACK manual 11.9.1).
      mapping_no_read.tab: x Mapping "regtobloc" has no `Read (by_elements)` statement assigning its values.
      mapping_undeclared_set.tab: x Set "BLOC" in the Mapping declaration of "REGTOBLOC" is not declared in the model.
      name_c_prefix.tab: x The `c_` prefix is reserved for change variables; rename coefficient "c_foo".
      name_coef_set_clash.tab: x Name declared as both a coefficient and a set: "reg". i TABLO names are case-insensitive and must be unique (GEMPACK manual 11.2.1).
      name_coef_var_clash.tab: x Name declared as both a coefficient and a variable: "pop". i TABLO names are case-insensitive and must be unique (GEMPACK manual 11.2.1).
      name_dup.tab: x Duplicate coefficient declaration: "dupx" (GEMPACK manual 11.2.1).
      name_overlength.tab: x Declaration name longer than 255 characters: "QQQQQQQQQQQQQQQQQQQQ...".
      name_reserved.tab: x Declaration name "max" is a reserved word (GEMPACK manual 11.2.1).
      postsim_scope.tab: x Ordinary statement references PostSim-declared name: "psx". i PostSim declarations are only visible inside PostSim sections (GEMPACK manual 12.2.1).
      postsim_unbalanced.tab: x Unbalanced PostSim section markers: 1 `PostSim (Begin)` against 0 `PostSim (End)` (GEMPACK manual 12.2).
      qual_empty.tab: x Empty qualifier `()` in declaration: "Variable () dummyv # empty qualifier #"
      qual_linear_name.tab: x The variable qualifiers `linear_name=` and `linear_var=` are not supported; use the default `p_`/`c_` linear name: "Variable (levels, linear_name=xlin) xlev # unsupported linear_name #"
      qual_unknown.tab: x Unknown declaration qualifier: "fob". i See GEMPACK manual 10.3/10.4 for the recognized variable and coefficient qualifiers.
      read_no_header.tab: x Read without a header is not supported (GEMPACK manual 11.11.8): "Read ELX from file GTAPDATA"
      read_terminal.tab: x Read from terminal is not supported; read from a file instead: "Read SCLR from terminal"
      read_undeclared.tab: x Read target "notdecl" not declared as a coefficient.
      set_ele_empty.tab: x Malformed element list for set SEL: "(x1,)" contains empty elements.
      set_ele_range.tab: x Element range abbreviation in set SRG: "s1 - s5". i The `(first - last)` form is not supported; list the elements explicitly.
      set_header_len.tab: x Header longer than 4 characters in the declaration of set REG: "TOOLONG".
      set_self_eq.tab: x Set SSE is defined as equal to itself (GEMPACK manual 10.1.2.1).
      set_self_ref.tab: x Set BADS references itself in its defining expression: "BADS + COMM". i Define a set from other sets and quoted elements only (GEMPACK manual 10.1.1.1).
      set_undeclared.tab: x Set referenced before declaration in "Set NMRG = COMM - MRG": "MRG". i Sets must be declared before they are used in a definition or Subset statement (GEMPACK manual 10.1).
      stmt_unknown_keyword.tab: x Equation statement without `=`: "Equation Frobnicate all the things" i Either the statement is malformed or its leading token is an unrecognized keyword that was read as an implicit Equation continuation.
      subset_undeclared.tab: x Set referenced before declaration in "Subset COMM is subset of KOMM": "KOMM". i Sets must be declared before they are used in a definition or Subset statement (GEMPACK manual 10.1).

