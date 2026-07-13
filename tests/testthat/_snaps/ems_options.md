# ems_option_get errors on invalid name

    `name` must be one of "verbose", "tempdir", "ndigits", "accuracy_threshold", "check_shock_status", "timestep_header", "n_timestep_header", "full_exclude", and "docker_tag", not "not_an_option".

# docker tag auto-selection

    Code
      tag <- .resolve_docker_tag()
    Message
      i Using image teems:v3 (matches host CPU capability "x86-64-v3"). Set `docker_tag` via `ems_option_set()` to override.

