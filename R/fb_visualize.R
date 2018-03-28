#' Visualize a summary of the field book data
#' 
#' @description 
#' Create histograms or boxplots of each trait in the field book table
#' 
#' @param fbt A Field Book Table (i.e. from reading in data using \code{read.fb})
#' @param traits \code{Character} column name of for which to plot heatmaps.
#' Defaults to all \code{numeric} traits.
#' @param graph The type of plot to produce. Accepted values are \code{"boxplot"}
#' or \code{"histogram"}.
#' @param bins The number of bins to force in the histogram.
#' @param nrow The number of graphs to display in a column. See 
#' \code{\link[ggplot2]{facet_wrap}}.
#' @param ncol The number of graphs to display in a row. See 
#' \code{\link[ggplot2]{facet_wrap}}.
#' 
#' @examples 
#' data("fbt_sample")
#' 
#' # For boxplots
#' fb_visualize(fbt = fbt_sample)
#' 
#' # For histograms
#' fb_visualize(fbt = fbt_sample, graph = "histogram")
#' 
#' 
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' 
#' @export
#' 
fb_visualize <- function(fbt, traits, graph = "boxplot", bins = 30, nrow = NULL, 
                         ncol = NULL) {
  
  ## Error handling

  # If traits is missing, set to default
  if (missing(traits)) {
    traits <- fb_traits(fbt = fbt, mode = "numeric")
    
  } else {
    if (!is.character(traits)) stop("'traits' must be a character.")
    
  }
  
  if (!is.character(graph)) stop("'graph' must be a character.")
  
  ## Reformat
  traits <- as.character(traits)
  graph <- as.character(graph)
  
  # Pull out the numeric traits
  num.traits <- fb_traits(fbt, mode = "numeric")
  
  # Make sure the traits are numeric
  if (any(!traits %in% num.traits))
    stop(paste(c(traits[!traits %in% num.traits], " is/are not among the numeric variables in the field.book.table.")))
  
  # Make sure graph is one of the accepted inputs
  if (!graph %in% c("boxplot", "histogram"))
    stop("The input for 'graph' is not accepted. Accepted inputs are 'boxplot'
         or 'histogram.'")
  
  ### Data tidying
  
  # Extract the traits, and gather
  fbt.gather <- fbt %>%
    select_(.dots = traits) %>%
    gather(trait, value)
  
  # Create the base graph
  gp <- fbt.gather %>%
    ggplot()
  
  if (graph == "boxplot") {
    
    gp1 <- gp + geom_boxplot(mapping = aes(trait, value)) +
      facet_wrap(~ trait, scales = "free", ncol = ncol, nrow = nrow)
    
  }
  
  if (graph == "histogram") {
    
    gp1 <- gp + geom_histogram(mapping = aes(value), bins = 30) +
      facet_wrap(~ trait, scales = "free", ncol = ncol, nrow = nrow)
    
  }
  
  # Print the graph
  print(gp1)
  
} # Close the function
