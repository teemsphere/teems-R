# name collisions abort

    x Name declared as both a coefficient and a variable: "psave".
    i TABLO names are case-insensitive and must be unique (GEMPACK manual 11.2.1).

---

    x Name declared as both a coefficient and a set: "reg".
    i TABLO names are case-insensitive and must be unique (GEMPACK manual 11.2.1).

---

    x Name declared as both a variable and a set: "reg".
    i TABLO names are case-insensitive and must be unique (GEMPACK manual 11.2.1).

# duplicate declarations abort

    x Duplicate coefficient declaration: "dupx" (GEMPACK manual 11.2.1).

# reserved words abort

    x Declaration name "max" is a reserved word (GEMPACK manual 11.2.1).

# c_ prefixed coefficients abort

    x The `c_` prefix is reserved for change variables; rename coefficient "c_foo".

# p_/c_ prefix clashes abort

    x Coefficient and variable pair sharing a base name: "vkb/p_vkb".
    i The generated `p_`/`c_` linear variable names do not disambiguate; rename one of each pair.

# over-length names abort

    x Declaration name longer than 255 characters: "AAAAAAAAAAAAAAAAAAAA...".

# unknown qualifiers abort

    x Unknown declaration qualifier: "foo".
    i See GEMPACK manual 10.3/10.4 for the recognized variable and coefficient qualifiers.

# no_split and linear_name qualifiers abort

    x The variable qualifier `no_split` (full shock at every step) is not supported: "Variable (no_split) dummyvar"

---

    x The variable qualifiers `linear_name=` and `linear_var=` are not supported; use the default `p_`/`c_` linear name: "Variable (levels, linear_name=xlin) dummyvar"

# empty qualifiers abort

    x Empty qualifier `()` in declaration: "Variable () dummyvar"

# duplicate bounds abort

    x Duplicate lower bound in declaration: "Coefficient (ge 0, ge 1) (all,r,REG) BNDP(r)"
    i One lower (`ge`/`gt`) and one upper (`le`/`lt`) bound are allowed per declaration (GEMPACK manual 10.19.1).

# invalid Default statements abort

    x Equation `(default=levels)` is not supported; the solver handles linearized equations only (GEMPACK manual 10.19): "Equation (default=levels)"

---

    x Coefficient bound defaults are not supported (GEMPACK manual 10.19): "Coefficient (default=lower_bound ge 0)"

---

    x Unknown Variable default "foo" (GEMPACK manual 10.19): "Variable (default=foo)"

---

    x Default statements apply only to Coefficient, Variable, Formula, and Equation declarations (GEMPACK manual 10.19): "Update (default=always)"

# solver-valid Default statements abort as unsupported

    x Default statements are not supported by the teems pipeline: "Variable (default=change)"
    i Declare the qualifier on each affected statement instead; the positional Default semantics (GEMPACK manual 10.19) cannot be carried through model preparation.

# self-referential set expressions abort

    x Set SBAD references itself in its defining expression: "SBAD + COMM".
    i Define a set from other sets and quoted elements only (GEMPACK manual 10.1.1.1).

# undeclared set references abort

    x Set referenced before declaration in "Set SND = COMM - MRGX": "MRGX".
    i Sets must be declared before they are used in a definition or Subset statement (GEMPACK manual 10.1).

---

    x Set referenced before declaration in "Set SEQ = NOPE": "NOPE".
    i Sets must be declared before they are used in a definition or Subset statement (GEMPACK manual 10.1).

---

    x Set referenced before declaration in "Subset REG is subset of NOPE2": "NOPE2".
    i Sets must be declared before they are used in a definition or Subset statement (GEMPACK manual 10.1).

# set self-equality aborts

    x Set SSE is defined as equal to itself (GEMPACK manual 10.1.2.1).

# element range abbreviations abort

    x Element range abbreviation in set SRG: "s1 - s5".
    i The `(first - last)` form is not supported; list the elements explicitly.

# malformed element lists abort

    x Malformed element list for set SEL: "()" contains empty elements.

---

    x Malformed element list for set SEL2: "(x1,)" contains empty elements.

# over-length set headers abort

    x Header longer than 4 characters in the declaration of set SHD: "TOOBIG".

# math statements without = abort

    x Formula statement without `=`: "Formula NOEQ 1"
    i Either the statement is malformed or its leading token is an unrecognized keyword that was read as an implicit Formula continuation.

---

    x Formula statement without `=`: "Formula Frobnicate all the things"
    i Either the statement is malformed or its leading token is an unrecognized keyword that was read as an implicit Formula continuation.

# Formula & Equation aborts

    x `Formula & Equation` statements are not supported: the expansion needs a levels equation (GEMPACK manual 10.9.1).
    i Linearize the equation and set the base value with `Formula (initial)`.

# headerless reads abort

    x Read without a header is not supported (GEMPACK manual 11.11.8): "Read ELX from file GTAPDATA"

# reads into undeclared names abort

    x Read target "notdecl" not declared as a coefficient.

# read from terminal aborts

    x Read from terminal is not supported; read from a file instead: "Read ELX from terminal"

# PostSim scope violations abort

    x Ordinary statement references PostSim-declared name: "pscalc".
    i PostSim declarations are only visible inside PostSim sections (GEMPACK manual 12.2.1).

# PostSim reads from ordinary files abort

    x File "gtapdata" read in both the ordinary and PostSim parts.
    i Split the data across two files (GEMPACK manual 12.2.3).

# PostSim reads into ordinary coefficients abort

    x PostSim Read into ordinary coefficient "save"; targets must be PostSim coefficients (GEMPACK manual 12.2.3).

# PostSim reads into variables abort

    x PostSim Read into variable "psave"; simulation results cannot be changed (GEMPACK manual 12.2.3).

# PostSim formulas assigning variables abort

    x PostSim Formula assigns variable "psave"; simulation results cannot be changed (GEMPACK manual 12.2.2).

# PostSim formulas assigning ordinary coefficients abort

    x PostSim Formula assigns ordinary coefficient "save"; the LHS must be a PostSim coefficient (GEMPACK manual 12.2.2).

