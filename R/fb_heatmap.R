#' Generate a heatmap of field data
#' 
#' @description Creates a heatmap image for each numeric trait in the field_book_table 
#' file. An option for exporting a PDF of these images is provided. This function works best with
#' raw field_book_table files, so this could be used immediately after exporting
#' a file from the Field Book App. 
#' 
#' @param fbt A Field Book Table object.
#' @param traits \code{Character} column name of for which to plot heatmaps.
#' Defaults to all \code{numeric} traits.
#' 
#' @import dplyr
#' @import ggplot2
#' @importFrom stats median
#' 
#' @export
#' 
fb_heatmap <- function(fbt, traits) {
  
  ## Error handling
  # If traits is missing, set to default
  if (missing(traits)) {
    traits <- fb_traits(fbt = fbt, mode = "numeric")
    
  } else {
    if (!is.character(traits)) stop("'traits' must be character.")
    
  }

  ## Reformat
  traits <- as.character(traits)

  # Pull out the numeric traits
  num.traits <- fb_traits(fbt, mode = "numeric")
  
  # Make sure the traits are numeric
  if (any(!traits %in% num.traits))
    stop(paste(c(traits[!traits %in% num.traits], " is/are not among the numeric variables in the field.book.table.")))

  
  ### Data manipulation
  
  # Gather the row and column information and the trait-value pairs
  fbt1 <- fbt[,c("row", "column", traits)]
  
  # Change row and column to factors
  fbt1$row <- as.factor(fbt1$row)
  fbt1$column <- as.factor(fbt1$column)
  
  # Iterate over traits
  for (tr in traits) {
    
    # Calculate the median
    values <- as.vector(fbt1[[tr]])
    tr_median <- median(x = values)
    
    row <- fbt1$row
    column <- fbt1$column
    
    # Plot
    gp <- ggplot(data = fbt1, aes(x = column, y = row, fill = values)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                           midpoint = tr_median) +
      ggtitle(tr)
    
    # Print the plot
    print(gp)
    
  }
  
} # Close the function
