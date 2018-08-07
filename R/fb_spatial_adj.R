#' Spatial adjustment of observations
#' 
#' @description
#' Uses a moving grid to adjust the phenotypic observations recorded in a Field Book 
#' table. This function wraps around functions provided in the \code{\link[mvngGrAd]{mvngGrAd}}
#' package.
#' 
#' @param fbt A Field Book Table object.
#' @param traits Traits to be adjusted. Defaults to all numeric traits.
#' @param checks A \code{character} of field.book.table entry names designated
#' as checks.
#' @param grid.size A nested \code{list} of grid dimensions that define the 
#' moving average grid. If \code{grid.size} is \code{NULL}, the grid size is 
#' optimized. See \emph{Details} for more information.
#' @param max.grid.size A nested \code{list}, similar to \code{grid.size}, but 
#' instead defining the maximum grid dimensions to use when optimizing the 
#' grid size. Here the components \code{grid.rows}, \code{grid.cols}, and 
#' \code{grid.layers} are integers defining the maximum rows, columns, and diagonal
#' layers to use, respectively. Use this option if the field dimensions are large.
#' This argument is ignored if \code{grid.size} is passed.
#' @param use.rel.eff If \code{TRUE}, the relative efficiency is
#' used to determine whether the adjusted data for a trait replaces the raw data
#' in the fbt object. If \code{FALSE}, the adjusted data replaces the raw data.
#' 
#' @details 
#' This function uses the moving average procedure implemented in \code{\link[mvngGrAd]{mvngGrAd}}
#' to spatially adjust the raw phenotypic observations from a grid. Briefly,
#' the function uses the mean of observations surrounding a particularly plot
#' in a field as a covariate to calculate the adjusted phenotypic value.
#' 
#' One can define the dimensions of the grid by passing a list to the \code{grid.size}
#' argument. The first layer of the list must be the same length as the number 
#' of traits, and the names of this layer must be the trait names. The second 
#' layer of the list must have the components \code{grid.rows}, \code{grid.cols}, 
#' and \code{grid.layers}. \code{grid.rows} and \code{grid.cols} define the
#' number of \emph{field} rows and columns covered by the grid, respectively,
#' while \code{grid.layers} defines the number of diagonal plots.
#' 
#' Alternatively, one can optimize the grid size for each trait. Grid optimization
#' iterates over all possible grid dimensions and identifies the grid size that
#' maximizes the correlation between the adjusted phenotypic values and the mean
#' of the phenotypic values within that grid.
#' 
#' Finally, the adjusted phenotypic values can be kept or ignored depending 
#' on whether the within-environment variance (i.e. \eqn{V_R}) is reduced after
#' adjustment. This function uses the individuals specified in \code{checks} to
#' calculate this variance. Generally, traits that are less heritable tend to 
#' benefit from spatial adjustment.
#' 
#' @return 
#' A list with two elements:
#' 
#' \describe{
#'   \item{fbt}{An fbt object with adjusted phenotypic values replacing the 
#'   original values, if applicable.}
#'   \item{summary}{A list with 2 elements: the first returns the grid size 
#'   used for each trait. The second returns summary statistics of the adjustment
#'   procedure, including the within-environment variance, relative efficiency,
#'   and whether the adjusted values were returned in place of the original values.}
#' }
#' 
#' @import mvngGrAd
#' @import dplyr
#' @importFrom  tidyr gather
#' @importFrom purrr map pmap transpose
#' @importFrom stats lm var sigma setNames
#' 
#' @examples 
#' data("fbt_sample")
#' 
#' # Specify a grid size
#' grid.size <- list(SpatialGradient = list(grid.rows = 5, grid.cols = 5, grid.layers = 5))
#' 
#' adj_out <- fb_spatial_adj(fbt = fbt_sample, traits = "SpatialGradient", 
#'                           checks = c("Kharkof", "TAM 107", "Scout 66"), 
#'                           grid.size = grid.size)                         
#'                           
#' \dontrun{
#' 
#' # Run spatial adjustment without conditions
#' adj_out <- fb_spatial_adj(fbt = fbt_sample, traits = "SpatialGradient", 
#'                           checks = c("Kharkof", "TAM 107", "Scout 66"))
#'                           
#' # Observe the output
#' 
#'                           
#' # Restrict the grid size
#' max.grid.size <- list(SpatialGradient = list(grid.rows = 5, grid.cols = 5, grid.layers = 5))
#' 
#' # Re-run spatial adjustment
#' adj_out <- fb_spatial_adj(fbt = fbt_sample, traits = "SpatialGradient", 
#'                           checks = c("Kharkof", "TAM 107", "Scout 66"), 
#'                           max.grid.size = max.grid.size)
#'    
#' }                       
#' 
#' @export
#' 
fb_spatial_adj <- function(fbt, traits, checks, grid.size = NULL, max.grid.size = NULL,
                           use.rel.eff = TRUE) {
  
  # If plot.traits is missing, set to default
  if (missing(traits)) {
    traits <- fb_traits(fbt = fbt, mode = "numeric")
    
  } else {
    if (!is.character(traits)) stop("'plot.traits' must be character.")
    
  }
  
  
  # Other arguments
  if (!is.character(checks)) stop("'checks' must be character.")  
  if (!is.logical(use.rel.eff)) stop("'use.rel.eff' must be logical.")
  
  # Pull out numeric traits
  num.traits <- fb_traits(fbt, mode = "numeric")
  
  # Make sure the specified traits are in the field.book.table
  if (any(!traits %in% num.traits))
    stop(paste(c(traits[!traits %in% num.traits], 
                 " is/are not among the numeric variables in the input fbt")))
  
  # Make sure the specified checks are in the fbt
  if (any(!checks %in% fbt$line_name))
    stop(paste(c(checks[!checks %in% fbt$line_name], 
                 " is/are not among the line names in input fbt")))
  
  # Extract row and column information
  rows <- fbt$row
  cols <- fbt$column
  
  # If grid.size is NULL, set grid opt to TRUE
  if (is.null(grid.size)) {
    
    grid.opt <- TRUE
    
    # There are four steps
    ## 1. Optimize with the full grid (including layers)
    ## 2. Optimize just rows and columns
    ## 3. Optimize just rows
    ## 4. Optimize just columns
    
    # Design a data.frame of possible grid sizes
    
    # All columns in the field
    lat <- seq(max(cols) - 1) 
    # All rows
    long <- seq(max(rows) - 1)
    
    grids <- data.frame(g.row = rep(long, each = length(lat)),
                        g.cols = rep(lat, length(long))) 
    # Add layers
    grids$g.layer <- pmin(grids$g.row, grids$g.cols)
    
    
    # Create a list of grids of length n_trait
    grid_list <- rep(list(grids), length(traits)) %>%
      setNames(nm = traits)
    
    # If max.grid.size is passed, use that information to restrict the grid sizes
    if (!is.null(max.grid.size)) {
      
      # First check it
      # Make sure it is of correct class
      if (!is.list(max.grid.size)) stop("'grid.size' must be a list.")
      
      # Make sure grid.size is the same length as the traits
      if (length(max.grid.size) != length(max.grid.size))
        stop("The length of max.grid.size is not equal to the number of traits.")
      
      # Make sure the names of the first layer are the trait names
      if (!all(names(max.grid.size) %in% traits))
        stop("The names of the first layer of 'max.grid.size' are not all traits.")
      
      # Make sure grid.size is formatted correctly
      grid.size.components <- c("grid.rows", "grid.cols", "grid.layers")
      
      for (tier in max.grid.size) {
        if (!any(names(tier) %in% grid.size.components))
          stop("The names of the second layer of 'max.grid.size' do not include 
               grid.rows, grid.col, and grid.layers")
      }
      
      # Sort the max.grid.size by trait
      max.grid.size <- max.grid.size[traits]
      
      # Determine the grid sizes that are less than the max grid size
      
    
      grid_list_use <- list(grid_list, max.grid.size) %>% 
        pmap(function(gr, max.gr) {
          grids_tokeep <- (gr$g.row <= max.gr$grid.rows & gr$g.cols <= max.gr$grid.cols & 
                             gr$g.layer <= max.gr$grid.layers)
          gr[grids_tokeep,,drop = FALSE]
          
        })
      
    } else {
      grid_list_use <- grid_list
      
    }
      
    
    # Oterwise set grid.opt to FALSE and check to make sure the appropriate
    ## grid components are present
  } else { 
    
    grid.opt <- FALSE
    
    # Make sure it is of correct class
    if (!is.list(grid.size)) stop("'grid.size' must be a list.")
    
    # Make sure grid.size is the same length as the traits
    if (length(grid.size) != length(traits))
      stop("The length of grid.size is not equal to the number of traits.")
    
    # Make sure the names of the first layer are the trait names
    if (!all(names(grid.size) %in% traits))
      stop("The names of the first layer of 'grid.size' are not all traits.")
    
    # Make sure grid.size is formatted correctly
    grid.size.components <- c("grid.rows", "grid.cols", "grid.layers")
    
    for (tier in grid.size) {
      if (!any(names(tier) %in% grid.size.components))
        stop("The names of the second layer of 'max.grid.size' do not include 
               grid.rows, grid.col, and grid.layers")
    }
    
    grid.size <- grid.size[traits]
    
  }

  
  # Extract line name information
  line_name <- fbt$line_name
  
  # Create an empty list to store the grid size and the moving average results
  trait_adjusted <- list()
  
  # Iterate over traits
  for (tr in traits) {
    
    # Subset the fbt object for the particular trait
    fbt_sub <- fbt[,c("unique_id", "row", "column", "line_name", tr)]
    # Extract the obervations
    p_obs <- as.vector(fbt_sub[[tr]])
  
    # Proceed with optimiztion if called
    if (grid.opt) {
      
      # Notify
      cat("\n\nOptimizing grid size for trait: ", tr, "\n")
      
      # Apply a function over the grids
      grids_cor <- apply(X = grid_list_use[[tr]], MARGIN = 1, FUN = function(i) {
        full_grid <- grid_cor(p_obs = p_obs, rows = rows, cols = cols, 
                              grid.rows = seq(i[1]), grid.cols = seq(i[2]), 
                              layers = seq(i[3]) )
        
        no_layers <- grid_cor(p_obs = p_obs, rows = rows, cols = cols, 
                              grid.rows = seq(i[1]), grid.cols = seq(i[2]), 
                              layers = NULL )
        
        only_cols <- grid_cor(p_obs = p_obs, rows = rows, cols = cols, 
                              grid.rows = NULL, grid.cols = seq(i[2]), 
                              layers = NULL )
        
        only_rows <- grid_cor(p_obs = p_obs, rows = rows, cols = cols, 
                              grid.rows = seq(i[1]), grid.cols = NULL, 
                              layers = NULL )
        
        data.frame(full_grid, no_layers, only_cols, only_rows)
        
      }) %>% bind_rows()
      
      
      ## Determine which grid scheme is optimial
      # Find the maximum correlation for each grid type
      opt_grid <- which.max(sapply(X = grids_cor, FUN = max))
      # What type of grid
      opt_grid_type <- names(grids_cor)[opt_grid]
      # Extract that grid
      grid_opt <- grid_list_use[[tr]][which.max(grids_cor[,opt_grid]),]
      
      
      # Depending on which grid scenario prevailed, set the optimal grid
      ## sizes for the moving average
      if (opt_grid_type == "full_grid") {
        grid.rows <- seq_len(grid_opt$g.row)
        grid.cols <- seq_len(grid_opt$g.cols)
        grid.layers <- seq_len(grid_opt$g.layer)
        
      } else if (opt_grid_type == "no_layers") {
        grid.rows <- seq_len(grid_opt$g.row)
        grid.cols <- seq_len(grid_opt$g.cols)
        grid.layers <- NULL
        
      } else if (opt_grid_type == "only_cols") {
        grid.rows <- NULL
        grid.cols <- seq_len(grid_opt$g.cols)
        grid.layers <- NULL
        
      } else if (opt_grid_type == "only_rows") {
        grid.rows <- seq_len(grid_opt$g.row)
        grid.cols <- NULL
        grid.layers <- NULL
      }
      
    # If grid optimization is not specified, proceed with the moving
    # average adjustment only
    } else {
      grid.rows <- grid.size[[tr]]$grid.rows
      grid.cols <- grid.size[[tr]]$grid.cols
      grid.layers <- grid.size[[tr]]$grid.layers
      
    }
    
    # Run the moving grid and Return the results
    mv.out <- movingGrid(rows = rows, columns = cols, obsPhe = p_obs,
                         shapeCross = list(
                           grid.rows,
                           grid.rows,
                           grid.cols,
                           grid.cols ),
                         layers = grid.layers, excludeCenter = TRUE)
    
    p_adj <- as.vector(fitted(mv.out))
    
    ## Add the original and adjusted data to the fbt data.frame
    fbt_adj <- cbind(fbt_sub, unadjusted = p_obs, adjusted = p_adj)
    
    # Measure residual variance by fitting a linear model with just the
    # checks
    p_obs_fit <- lm(unadjusted ~ line_name, data = fbt_adj, subset = line_name %in% checks)
    V_R_p_obs <- sigma(p_obs_fit)^2
    
    p_adj_fit <- lm(adjusted ~ line_name, data = fbt_adj, subset = line_name %in% checks)
    V_R_p_adj <- sigma(p_adj_fit)^2
    
    # Calculate the relative efficiency
    # The relative efficiency is greater than 1 when the unadjusted V_R is greater
    # than the adjusted V_R (good) and is less than 1 when the unadjusted V_R is less
    # than the adjusted V_R (bad)
    rel_eff <- V_R_p_obs / V_R_p_adj
    
    # Determine which vector of data to return based on relative efficiency 
    # (if specified)
    if (use.rel.eff) {
      
      # Only use the adjusted values if it improves the V_R (if it's the same,
      # use the original observations)
      if (rel_eff > 1) {
        fbt[[tr]] <- p_adj
        use_p_adj <- TRUE
        
      } else {
        fbt[[tr]] <- p_obs
        use_p_adj <- FALSE
      }
        
    # If the relative efficiency is not used, return the adjusted value
    } else {
      fbt[[tr]] <- p_adj
      use_p_adj <- TRUE
      
    } # Close the if else code
    
    ## Create a list with the grid sizes and variances
    grid_df <- data.frame(trait = tr, 
                          grid.rows = ifelse(is.null(grid.rows), 0, grid.rows),
                          grid.cols = ifelse(is.null(grid.cols), 0, grid.cols),
                          grid.layers = ifelse(is.null(grid.layers), 0, grid.layers),
                          stringsAsFactors = FALSE)
    
    summary_df <- data.frame(trait = tr,
                             V_R_unadjusted = V_R_p_obs, V_R_adjusted = V_R_p_adj,
                             relative_eff = rel_eff, use_adjusted = use_p_adj,
                             stringsAsFactors = FALSE)
    
    trait_adjusted[[tr]] <- list(grid_size = grid_df, adjustment_summary = summary_df)
    
    
  } # Close the for loop
  
  # Collapse the list
  trait_adjusted1 <- transpose(trait_adjusted) %>% 
    map(bind_rows)
  

  # Return data
  return.list <- list(fbt = fbt, summary = trait_adjusted1)
  return(return.list)
    
} # Close the function
