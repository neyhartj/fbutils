#' Extract the trait names from a Field Book Table
#' 
#' @description 
#' Returns a character vector of trait names of the desired mode
#' 
#' @param fbt A \code{field.book.table} object.
#' @param mode The class of traits to be returned. May be \code{character}, 
#' \code{numeric} or \code{logical}.
#' 
#' @import dplyr
#' @import stringr
#' 
#' @export
#' 
fb_traits <- function(fbt, mode = NULL) {
  
  # Mode cannot be of length > 1
  if (length(mode) >  1) stop("'mode' cannot have length > 1.")
  
  # Make sure the type argument is within the accepted inputs, if it isn't NULL
  if (!is.null(mode)) {
    
    if (!mode %in% c("numeric", "character", "logical"))
      stop ("'mode' must be 'numeric,' 'character,' or 'logical.'")
    
  } else {
    mode = "all"
  }
  
  # Pull out the traits by those columns that are capitalized
  traits <- names(fbt) %>% 
    str_subset(pattern = "^[A-Z]")
  
  # If any mode is desired, return all traits
  if (mode == "all") {
    
    return(traits)
    
  # Otherwise select the traits matching the class of 'mode'
  } else {
    
    # Control flow by mode
    if (mode == "numeric") {
      
      # Summarize each column by checking the class
      col.summary <- fbt %>%
        summarize_all(funs(is.numeric)) %>%
        select_(.dots = traits)
    
      traits.return <- names(col.summary)[col.summary == "TRUE"]
      
    }
    if (mode == "character") {
      
      # Summarize each column by checking the class
      col.summary <- fbt %>%
        summarize_all(funs(is.character)) %>%
        select_(.dots = traits)
      
      traits.return <- names(col.summary)[col.summary == "TRUE"]
      
    }
    if (mode == "logical") {
      
      # Summarize each column by checking the class
      col.summary <- fbt %>%
        summarize_all(funs(is.logical)) %>%
        select_(.dots = traits)
      
      traits.return <- names(col.summary)[col.summary == "TRUE"]
      
    }
    
    return(traits.return)
    
  }
  
} # Close the function