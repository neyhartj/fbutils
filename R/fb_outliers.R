#' Search for outliers in the field data
#' 
#' @param fbt A Field Book Table object.
#' @param traits A \code{character} of traits for which outliers should be 
#' discovered. By default all numeric traits are examined.
#' @param max.sd A \code{numeric} determining the number of standard deviations from
#' the mean to use when labelling outliers.
#' 
#' @import dplyr
#' @importFrom stats sd
#' 
#' @export
#' 
fb_outliers <- function(fbt, traits, max.sd = 3) {
  
  ### Error check
  # If traits is missing, set to default
  if (missing(traits)) {
    traits <- fb_traits(fbt = fbt, "numeric")
    
  } else {
    if (!is.character(traits)) stop("'traits' must be character.")
    
  }
  
  # Other arguments
  if (!is.numeric(max.sd)) stop("'max.sd' must be numeric.")

  # Reformat
  max.sd <- as.numeric(max.sd)
  traits <- as.character(traits)
  
  ### Load Data
  # Pull out the numeric traits
  num.traits <- fb_traits(fbt, "numeric")
  
  # Make sure the traits are numeric
  if (any(!traits %in% num.traits))
    stop(paste(c(traits[!traits %in% num.traits], 
                 " is/are not among the numeric variables in the field.book.table.")))
                 
  # Create an empty list for the outlier summary
  outlier_summary <- list()
  
  # Iterate over traits
  for (tr in traits) {
    
    # Pull out the data on those traits
    tr_values <- as.vector(fbt[[tr]])
    
    # Calculate summaries
    tr_mean <- mean(tr_values, na.rm = TRUE)
    tr_sd <- sd(tr_values, na.rm = TRUE)
    n_NA <- sum(is.na(tr_values))
    
    # Calculate upper and lower bounds
    upper_bound <- tr_mean + (tr_sd * max.sd)
    lower_bound <- tr_mean - (tr_sd * max.sd)
    
    ## Detect outliers
    is_outlier <- tr_values < lower_bound | tr_values > upper_bound
    
    # Subset the data for those outliers
    fbt_trait <- fbt[,names(fbt) %in% c("unique_id", "row", "column", "line_name", tr)]
    fbt_outliers <- as.data.frame(fbt_trait[is_outlier,,drop = FALSE])
    
    # Notify user
    cat("\n\nOutlier summary for trait: ", tr)
    cat("\nSample mean:", tr_mean)
    cat("\nSample standard deviation: ", tr_sd)
    cat("\nNumber of missing data points: ", n_NA)
    cat("\nUpper bouund: ", upper_bound)
    cat("\nLower bound: ", lower_bound, "\n\n")
    
    # Print if outliers
    if (nrow(fbt_outliers) > 0) {
      print(fbt_outliers)
      
    } else {
      cat("No outliers detected.")
      
    }
    
    ## Add the outliers to the list
    outlier_summary[[tr]] <- fbt_outliers
    
  } # Close the loop

  # Return the data
  return(outlier_summary)

} # Close the function
  
  