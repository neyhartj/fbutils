#' Extract the trait names from a Field Book Table
#' 
#' @description 
#' Returns a character vector of trait names of the desired mode
#' 
#' @param fbt A \code{field.book.table} object.
#' @param mode The class of traits to be returned. May be \code{"all"} (default),
#' \code{"character"}, \code{"numeric"} or \code{"logical"}.
#' 
#' @export
#' 
fb_traits <- function(fbt, mode = c("all", "character", "numeric", "logical")) {
  
  # Match arguments for the mode
  mode <- match.arg(mode)
  
  # Pull out the traits by those columns that are capitalized
  trait_index <- grepl(pattern = "^[A-Z]", x = names(fbt))
  traits <- names(fbt)[trait_index]
  
  # If any mode is desired, return all traits
  if (mode == "all") {
    traits_return <- traits
    
  # Otherwise select the traits matching the class of 'mode'
  } else {
    # Get the classes of each trait 
    trait_classes <- sapply(X = fbt[traits], class)
    
    # Return the traits that match the mode
    if (mode == "numeric") {
      # For numeric, return doubles or integers
      traits_return <- traits[trait_classes %in% c("numeric", "integer")]
      
    } else {
      traits_return <- traits[trait_classes == mode]
      
    }
  } # Close the if statement
  
  # Return the traits
  return(traits_return)
  
} # Close the function