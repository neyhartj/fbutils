#' Parse data from non-standard traits
#' 
#' @param fbt A Field Book Table object.
#' @param traits A \code{character} vector of trait names identifying which column(s) in the
#' field_book_table.csv file to parse.
#' @param sep Character(s) used to split the multicat data entries.
#' @param n.obs An \code{numeric} vector of integers describing the number
#' of desired datapoint observations in a multicat trait. The first n observations will be
#' included and the remainder discarded For instance, if one of the
#' multicat data points was "1:2:3:4:5" and n.obs was set to 3, the resulting
#' data would be "1 2 3". The vector must be the same length as 'traits.'
#' 
#' @return A \code{data.frame} of the original field_book_table.csv data but with
#' individual columns for each datapoint entry in each multicat trait. The mean and
#' standard deviation of all datapoint entries are also included.
#' 
#' @import dplyr
#' @import stringr
#' @import tidyr
#' 
#' @export
#' 
fb_parse <- function(fbt, traits, sep = ":", n.obs = 10) {
  
  # Other arguments
  if (!is.numeric(n.obs)) stop("'n.obs' must be numeric.")
  if (!is.character(traits)) stop("'traits' must be character.")
  if (!is.character(sep)) stop("'sep' must be character.")
  
  # Convert to class
  n.obs <- as.numeric(n.obs)
  traits <- as.character(traits)
  sep <- as.character(sep)
  
  # Pull all of the traits
  fbt.traits <- fb_traits(fbt)
  
  # Make sure the traits are in the fbt
  if (any(!traits %in% fbt.traits))
    stop(paste(c(traits[!traits %in% fbt.traits], 
                 " is/are not among the numeric variables in the input x")))
                 
  
  
  # Iterate over trait columns
  parsed.data <- lapply(X = traits, FUN = function(trait) {
    
    # Subset the fbt for the trait
    fbt.sub <- fbt %>%
      select_(trait) %>%
      # Use separate to parse
      separate_(col = trait, into = str_c(trait, seq(n.obs)), sep = sep,
      convert = TRUE, extra = "drop", fill = "right")
    
    # Calculate mean and sd
    fbt.sub.stat <- apply(X = fbt.sub, MARGIN = 1, 
                          FUN = function(row) c(mean = mean(row), sd = sd(row))) %>% 
      t() %>%
      as.data.frame()
    
    names(fbt.sub.stat) <- paste(trait, names(fbt.sub.stat), sep = "_")
    
    fbt.sub1 <- bind_cols(fbt.sub, fbt.sub.stat)
    
    # Return
    return(fbt.sub1) })
  
  # Rbind the list
  parsed.data1 <- parsed.data %>%
    bind_cols()
  
  # Assemble new data.frame and return it
  fbt %>%
    select(which(!colnames(.) %in% traits)) %>%
    bind_cols(., parsed.data1)

} # Close the function
