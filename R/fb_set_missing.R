#' Set trait values to NA
#' 
#' @description 
#' Sets observations to missing (NA) for specified traits
#' 
#' @param fbt A Field Book Table object.
#' @param traits A \code{character} vector of traits for which values should
#' be set to missing. Defaults to all traits.
#' @param unique_ids A \code{character} vector of unique identifiers for which
#' observations should be set to missing.
#' 
#' @return 
#' An fbt object with the specified observations set to missing (i.e. \code{NA})
#' 
#' @examples 
#' data("fbt_sample")
#' 
#' # Detect the outlier in the 'OneOutlier' trait using 'fb_outliers()'
#' outliers <- fb_outliers(fbt = fbt_sample, traits = "OneOutlier")
#' 
#' # Set this outlier to NA
#' fbt1 <- fb_set_missing(fbt = fbt_sample, traits = "OneOutlier", unique_ids = "13RPN00010")
#'  
#' @export
#' 
fb_set_missing <- function(fbt, traits, unique_ids) {
  
  if (missing(traits)) {
    traits <- fb_traits(fbt)
    
  } else {
    if (!is.character(traits)) stop("'traits' must be character.")
    
  }
  
  # Pull out the numeric traits
  fbt.traits <- fb_traits(fbt)
  
  # Make sure the traits are numeric
  if (any(!traits %in% fbt.traits))
    stop(paste(c(traits[!traits %in% fbt.traits], " is/are not among the traits in the fbt.")))
  
  if (!is.character(unique_ids)) stop("'unique_ids' must be character.")
  
  # Make sure the unique_ids are within the line_names
  if (any(!unique_ids %in% fbt$unique_id))
    stop("The input 'unique_ids' is/are not in the fbt.")
  
  
  # Set to missing
  fbt[fbt$unique_id %in% unique_ids, traits] <- NA
  
  # Return the fbt
  return(fbt)
  
} # Close the function