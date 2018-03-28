#' Spatial adjustment of observations
#' 
#' @description
#' Uses a moving grid to adjust the phenotypic observations
#' recorded in a Field Book table. This function wraps around 
#' functions provided in the \code{\link[mvngGrAd]{mvngGrAd}}
#' package.
#' 
#' @param fbt A Field Book Table object.
#' @param traits Traits to be adjusted. Defaults to all numeric traits.
#' @param checks A \code{character} of field.book.table entry names designated
#' as checks.
#' @param grid.size A nested \code{list} of grid dimensions that define the 
#' moving average grid. The first layer of the list must be the same length
#' as the number of traits, and the names of this layer must be the trait names.
#' The second layer of the list must have the components \code{grid.rows}, 
#' \code{grid.cols}, and \code{grid.layers}. See \code{Details} for what these 
#' indicate. If \code{grid.size} is \code{NULL}, the grid size is optimized.
#' @param max.grid.size A nested \code{list}, similar to \code{grid.size}, but 
#' instead defining the maximum grid dimensions to use when optimizing the 
#' grid size. Here the components \code{grid.rows}, \code{grid.cols}, and 
#' \code{grid.layers} are integers defining the maximum rows, columns, and diagonal
#' layers to use, respectively. Use this option if the field dimensions are large.
#' This argument is ignored if \code{grid.size} is passed.
#' @param use.rel.eff \code{Logical} If \code{TRUE}, the relative efficiency is
#' used to determine whether the adjusted data for a trait replaces the raw data
#' in the \code{field.book.table}. If \code{FALSE}, both raw and adjusted data
#' is added to the \code{field.book.table}.
#' 
#' @details 
#' Details to come
#' 
#' 
#' @import mvngGrAd
#' @import dplyr
#' @import stringr
#' @importFrom  tidyr gather
#' @importFrom purrr map pmap
#' @importFrom stats lm var sigma
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
    stop(str_c(str_c(traits[!traits %in% num.traits], collapse = " "), 
               " is/are not among the numeric variables in x."))
  
  # Make sure the specified checks are in the fbt
  if (any(!checks %in% fbt$line_name))
    stop(str_c(str_c(checks[!checks %in% fbt$line_name], collapse = " "), 
               " is/are not among the line names in x."))
  
  
  # Extract row and column information
  rows <- fbt$row
  cols <- fbt$column
  
  # If grid.size is NULL, set grid opt to TRUE
  if (is.null(grid.size)) {
    
    grid.opt = TRUE
    
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
                        g.cols = rep(lat, length(long)) ) %>%
      # Add layers
      rowwise() %>% 
      mutate(g.layer = min(g.row, g.cols)) %>%
      ungroup()
    
    # Create a list of grids of length n_trait
    grid_list <- rep(list(grids), length(traits)) %>%
      structure(names = traits)
    
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
    
      grid_list_use <- list(grid_list, max.grid.size) %>% 
        pmap(function(gr, max.gr) 
          filter(gr, g.row <= max.gr$grid.rows, 
                 g.cols <= max.gr$grid.cols, 
                 g.layer <= max.gr$grid.layers))
      
    } else {
      grid_list_use <- grid_list
      
    }
      
    
    # Oterwise set grid.opt to FALSE and check to make sure the appropriate
    ## grid components are present
  } else { 
    
    grid.opt = FALSE
    
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

  ## Gather the data in the fbt by trait
  fbt_tidy <- fbt %>% 
    select_("unique_id", "row", "column", "line_name", .dots = traits) %>% 
    gather(trait, value, -unique_id:-line_name)
  
  # Proceed with optimiztion if called
  if (grid.opt) {
  
    # Split the tidy data into traits and apply the function
    mvg_out_list <- fbt_tidy %>%
      split(.$trait) %>%
      list(., grid_list_use) %>%
      pmap(function(fbt_trait, gr_list) {
        
        # Trait name
        trait_i <- unique(fbt_trait$trait)
      
        # Extract the data
        p_obs <- fbt_trait %>%
          pull(value) %>% 
          as.matrix()

        # Notify
        cat("\n\nOptimizing grid size for trait: ", trait_i, "\n")
        
        # Apply a function over the grids
        grids_cor <- gr_list %>%
          group_by(g.row, g.cols, g.layer) %>% 
          do({
            full_grid <- grid_cor(p_obs = p_obs, rows = rows, cols = cols, 
                                  grid.rows = seq(.$g.row), grid.cols = seq(.$g.cols), 
                                  layers = seq(.$g.layer) ) 
            
            no_layers <- grid_cor(p_obs = p_obs, rows = rows, cols = cols, 
                                  grid.rows = seq(.$g.row), grid.cols = seq(.$g.cols), 
                                  layers = NULL )
            
            only_cols <- grid_cor(p_obs = p_obs, rows = rows, cols = cols, 
                                  grid.rows = NULL, grid.cols = seq(.$g.cols), 
                                  layers = NULL )
            
            only_rows <- grid_cor(p_obs = p_obs, rows = rows, cols = cols, 
                                  grid.rows = seq(.$g.row), grid.cols = NULL, 
                                  layers = NULL )
            
            data.frame(full_grid, no_layers, only_cols, only_rows) }) %>%
          ungroup()
        
        ## Determine which grid scheme is optimial
        # Find the maximum correlation for each grid type
        opt_grid <- grids_cor %>% 
          gather(grid_type, value, -g.row, -g.cols, -g.layer) %>% 
          top_n(n = 1, wt = value) %>%
          # If a tie, take the smallest grid
          head(1)
        
        # Depending on which grid scenario prevailed, set the optimal grid
        ## sizes for the moving average
        if (opt_grid$grid_type == "full_grid") {
          grid.rows <- seq_len(opt_grid$g.row)
          grid.cols <- seq_len(opt_grid$g.cols)
          grid.layers <- seq_len(opt_grid$g.layer)
        
        } else if (opt_grid$grid_type == "no_layers") {
          grid.rows <- seq_len(opt_grid$g.row)
          grid.cols <- seq_len(opt_grid$g.cols)
          grid.layers <- NULL
        
        } else if (opt_grid$grid_type == "only_cols") {
          grid.rows <- NULL
          grid.cols <- seq_len(opt_grid$g.cols)
          grid.layers <- NULL
        
        } else if (opt_grid$grid_type == "only_rows") {
          grid.rows <- seq_len(opt_grid$g.row)
          grid.cols <- NULL
          grid.layers <- NULL
        }
        
        # Run the moving grid and Return the results
        mv.out <- movingGrid(rows = rows, columns = cols, obsPhe = p_obs,
                   shapeCross = list(
                     grid.rows,
                     grid.rows,
                     grid.cols,
                     grid.cols ),
                   layers = grid.layers, excludeCenter = TRUE)
        
        p_adj <- fitted(mv.out) %>% 
          as.vector()
        
        # Add the trait to the fbt
        fbt_trait_x <- fbt_trait %>%
          mutate(type = "unadjusted")
        
        fbt_trait_y <- fbt_trait %>% 
          mutate(value = p_adj, 
                 type = "adjusted")
        
        fbt_trait1 <- full_join(x = fbt_trait_x, y = fbt_trait_y, 
                                by = c("unique_id", "row", "column", "line_name", "trait", "value", "type"))
          
        # Return
        list(fbt.trait = fbt_trait1,
             grid.size = list(grid.rows = grid.rows, grid.cols = grid.cols, grid.layers = grid.layers))
      
    })
    
  # No grid optimiation
  } else {
    
    # Split the tidy data into traits and apply the function
    mvg_out_list <- fbt_tidy %>%
      split(.$trait) %>%
      list(., grid.size) %>%
      pmap(function(fbt_trait, gr_list) {
        
        # Trait name
        trait_i <- unique(fbt_trait$trait)
        
        # Extract the data
        p_obs <- fbt_trait %>%
          pull(value) %>% 
          as.matrix()
  
        # Prepare the grid sizes
        grid.dims <- gr_list %>% 
          map(seq)
        
        # Run the moving grid and Return the results
        mv.out <- movingGrid(rows = rows, columns = cols, obsPhe = p_obs,
                             shapeCross = list(
                               grid.dims$grid.rows,
                               grid.dims$grid.rows,
                               grid.dims$grid.cols,
                               grid.dims$grid.cols ),
                             layers = grid.dims$grid.layers, excludeCenter = TRUE)
        
        p_adj <- fitted(mv.out) %>% 
          as.vector()
        
        # Add the trait to the fbt
        fbt_trait_x <- fbt_trait %>%
          mutate(type = "unadjusted")
        
        fbt_trait_y <- fbt_trait %>% 
          mutate(value = p_adj, 
                 type = "adjusted")
        
        fbt_trait1 <- full_join(x = fbt_trait_x, y = fbt_trait_y, 
                                by = c("unique_id", "row", "column", "line_name", "trait", "value", "type"))
        
        # Return
        list(fbt.trait = fbt_trait1,
             grid.size = grid.dims)
        
      })
    
  }
  
  ## Recombine the fbt
  fbt.tidy1 <- lapply(X = mvg_out_list, FUN = function(i) i$fbt.trait) %>%
    bind_rows()
  
  # Measure the phenotypic variance
  fbt.summary1 <- fbt.tidy1 %>%
    group_by(trait, type) %>%
    summarize(V_P = var(value, na.rm = T))
  
  # Measure residual variance
  fbt.summary2 <- fbt.tidy1 %>%
    filter(line_name %in% checks) %>%
    group_by(trait, type) %>%
    do(fit = lm(value ~ line_name, data = .)) %>%
    bind_cols(., summarize(., V_R = sigma(fit) ^ 2)) %>%
    select(-fit)
    
  # Combine and calculate relative efficiency
  fbt.adj.summary <- full_join(fbt.summary1, fbt.summary2, by = c("trait", "type"))
  # Calculate relative efficiency
  fbt.adj.summary <- fbt.adj.summary %>%
    full_join(., fbt.adj.summary %>%
                select(., -V_P) %>% 
                spread(type, V_R) %>% 
                summarize(rel_eff = unadjusted / adjusted),
              by = "trait") %>%
    
    # Ungroup
    ungroup()
  
  # Select the traits to keep adjusted data by observing the relative efficiency
  if (use.rel.eff) {
    
    # Trait to keep spatially-adjusted data
    trait.to.keep <- fbt.adj.summary %>% 
      filter(rel_eff > 1) %>% 
      select(trait) %>% 
      distinct() %>%
      as.matrix() %>%
      as.character()
    
    # Otherwise choose the spatial adjustment for all traits
  } else {
    
    trait.to.keep <- fbt.adj.summary %>% 
      select(trait) %>% 
      distinct() %>%
      as.matrix() %>%
      as.character()
  }
  
  # If no traits are to be kept, output the original fbt
  if (length(trait.to.keep) == 0) {
    
    fbt2 <- fbt
    
  } else {
    
    # Subset the adjusted data for those traits
    fbt2 <- fbt.tidy1 %>% 
      filter(trait %in% trait.to.keep, type == "adjusted") %>% 
      select(-type) %>% 
      spread(trait, value) %>%
      full_join(x = fbt, y = ., by = c("unique_id", "row", "column", "line_name")) %>%
      
      # Select the traits that end in y (i.e. the adjusted traits)
      select(-ends_with(".x"))
    
    # Rename
    names(fbt2) <- names(fbt2) %>% 
      str_replace_all(pattern = "\\.y", replacement = "")
    
  }
    
    
    # Get the grid sizes from the results
    grid.specs <- sapply(X = mvg_out_list, FUN = function(i) 
      # Return the number of rows, cols, layers, etc
      sapply(i$grid.size, length) )
      
    
    # Return data
    return.list <- list(fbt.adj = fbt2, summary = fbt.adj.summary, 
                        grid.size = grid.specs)
    return(return.list)
    
} # Close the function
