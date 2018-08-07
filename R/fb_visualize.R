#' Visualize a summary of the field book data
#' 
#' @description 
#' Create histograms or boxplots of each trait in the field book table
#' 
#' @param fbt A Field Book Table object.
#' @param traits A character vector specifying the trait to plot. Defaults to all
#' numeric traits.
#' @param graph The type of plot to produce. Accepted values are \code{"boxplot"}
#' (default) or \code{"histogram"}.
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
fb_visualize <- function(fbt, traits, graph = c("boxplot", "histogram"), bins = 30, 
                         nrow = NULL, ncol = NULL) {
  
  ## Error handling

  # If traits is missing, set to default
  if (missing(traits)) {
    traits <- fb_traits(fbt = fbt, mode = "numeric")
    
  } else {
    if (!is.character(traits)) stop("'traits' must be a character.")
    
  }
  
  if (!is.character(graph)) stop("'graph' must be a character.")
  
  # Match arguments for graph
  graph <- match.arg(graph)
  
  ## Reformat
  traits <- as.character(traits)
  graph <- as.character(graph)
  
  # Pull out the numeric traits
  num.traits <- fb_traits(fbt, mode = "numeric")
  
  # Make sure the traits are numeric
  if (any(!traits %in% num.traits))
    stop(paste(c(traits[!traits %in% num.traits], " is/are not among the numeric variables in the field.book.table.")))
  
  ## Tidy the data
  
  # Extract the traits, and gather
  fbt.gather <- gather_(fbt[,traits], "trait", "value", traits)
  fbt.gather1 <- subset(fbt.gather, !is.na(value))
  
  # Extract the columns
  trait <- fbt.gather1$trait
  value <- fbt.gather1$value
  
  
  if (graph == "boxplot") {
    
    gp <- qplot(x = trait, y = value, geom = graph, data = fbt.gather1) +
      facet_wrap(~ trait, scales = "free", ncol = ncol, nrow = nrow)
    
  }
  
  if (graph == "histogram") {
    
    gp <- qplot(x = value, geom = graph, data = fbt.gather1, bins = bins) +
      facet_wrap(~ trait, scales = "free", ncol = ncol, nrow = nrow)
    
  }
  
  # Print the graph
  print(gp1)
  
} # Close the function
