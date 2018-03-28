#' Convenience functions
#' 
#' @description 
#' Convenience functions not generally to be called by the user.
#' 
#' @importFrom mvngGrAd movingGrid
#' 
grid_cor <- function(p_obs, rows, cols, grid.rows, grid.cols, layers) {
  mg.results <- suppressWarnings({
    movingGrid(rows = rows, columns = cols, obsPhe = p_obs, 
               shapeCross = list(
                 grid.rows, grid.rows, # Rows up and down
                 grid.cols, grid.cols ), # Cols left and right
               layers = layers, excludeCenter = TRUE) })
  
  # Return the correlation between the observed phenotypes
  ## and the moving means
  return(mg.results@correlation)
  
} # Close the function