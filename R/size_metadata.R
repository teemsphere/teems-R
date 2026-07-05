#' @importFrom purrr map_dbl
#'
#' @keywords internal
#' @noRd
.compute_size_metadata <- function(var_extract,
                                   sets,
                                   closure) {
  n_var_ele <- sum(purrr::map_dbl(
    var_extract$ls_upper_idx,
    function(var_sets) {
      if (var_sets %=% NA) {
        return(1)
      }
      prod(lengths(with(sets$ele, mget(var_sets, ifnotfound = ""))))
    }
  ))

  n_exo_ele <- sum(purrr::map_dbl(
    closure,
    function(entry) {
      ele <- attr(entry, "ele")
      if (ele %=% NA) {
        return(1)
      }
      nrow(ele)
    }
  ))

  list(
    system_size = n_var_ele - n_exo_ele,
    n_reg = length(sets$ele$REG)
  )
}
