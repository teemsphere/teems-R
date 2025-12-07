#' @importFrom data.table as.data.table
#' 
#' @keywords internal
#' @noRd
.slice_array <- function(arr,
                         dim_sizes) {
  n_dims <- length(dim_sizes)
  other_dims <- setdiff(1:n_dims, c(1, 2))
  other_indices <- lapply(dim_sizes[other_dims], seq_len)
  other_grid <- expand.grid(other_indices)

  ls_mat <- list()
  for (i in 1:nrow(other_grid)) {
    indices <- as.list(other_grid[i, ])
    full_indices <- vector("list", n_dims)
    full_indices[c(1, 2)] <- list(quote(expr = ))
    full_indices[other_dims] <- indices
    ls_mat[[i]] <- do.call("[", c(list(arr), full_indices))
  }
  ls_dt <- lapply(
    ls_mat,
    function(m) {
      data.table::as.data.table(x = rbind(m))
    }
  )
  return(ls_dt)
}
