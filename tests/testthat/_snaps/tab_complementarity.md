# malformed complementarity aborts

    x Malformed Complementarity statement: "Complementarity (variable = CX, wrong_key = 0) CMPA CX - 3;"
    i Expected `Complementarity (variable = <levels var>, lower_bound/upper_bound = <levels var | parameter | constant>) <name> [quantifiers] <expression>;` (GEMPACK manual 10.17).

# missing variable qualifier aborts

    x Complementarity "Complementarity (lower_bound = 0) CMPA CY - 3;" needs a `variable =` qualifier (GEMPACK manual 11.14).

# non-levels complementarity variable aborts

    x The Complementarity variable "qgdp" must be a declared levels variable (GEMPACK manual 11.14).

# missing bound aborts

    x Complementarity "CMPA" needs at least one of `lower_bound`/`upper_bound` (GEMPACK manual 10.17).

# invalid bound aborts

    x Invalid bound "NPB" in Complementarity "CMPA".
    i A bound must be a levels variable, a `Coefficient (parameter)` or a real constant (GEMPACK manual 10.17).

# long complementarity name aborts

    x Complementarity name "CMPTOOLONGX" exceeds the 10-character limit (GEMPACK manual 11.14/11.2.1).

# quantifier count mismatch aborts

    x Complementarity "CMPA" has 0 quantifiers but "QX" has 1 argument (GEMPACK manual 11.14).

# condensed complementarity variable aborts

    x "CX" cannot be omitted: it is the variable of Complementarity "CMPA".
    i The complementarity variable must not be omitted, substituted out or backsolved; bound variables must not be omitted or substituted out (GEMPACK manual 11.14.1).

# endogenous complementarity variable aborts at deploy

    x Complementarity "CMPA": variable "CX" is not exogenous over its full domain (0 of 1 element exogenous after swaps).
    i The solver's complementarity state machinery is not implemented yet (C2); a complementarity currently deploys only with its variable fully exogenous.

