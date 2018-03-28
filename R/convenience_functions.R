#' Convenience functions
#' 
#' @description 
#' Convenience functions not generally to be called by the user.
#' 
#' @param p_obs The phenotypic observations
#' @param rows Number of field rows
#' @param cols Number of field columns
#' @param grid.rows Number of grid rows
#' @param grid.cols Number of grid columns
#' @param layers Number of grid layers
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