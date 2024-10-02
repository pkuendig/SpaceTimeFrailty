#source: https://github.com/fabsig/GPBoost/blob/master/R-package/R/gpb.cv.R

get.grid.size <- function(param_grid) {
  # Determine total number of parameter combinations on a grid
  # Author: Fabio Sigrist
  grid_size = 1
  for (param in param_grid) {
    grid_size = grid_size * length(param)
  }
  return(grid_size)
}

get.param.combination <- function(param_comb_number, param_grid) {
  # Select parameter combination from a grid of parameters
  # param_comb_number: Index number of parameter combination on parameter grid that should be returned (counting starts at 0)
  # Author: Fabio Sigrist
  param_comb = list()
  nk = param_comb_number
  for (param_name in names(param_grid)) {
    ind_p = nk %% length(param_grid[[param_name]])
    param_comb[[param_name]] = param_grid[[param_name]][ind_p + 1]
    nk = (nk - ind_p) / length(param_grid[[param_name]])
  }
  return(param_comb)
}
