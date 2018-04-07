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
                 " is/are not among the numeric variables in the input fbt")))
                 
  
  
  # Iterate over trait columns
  parsed.data <- lapply(X = traits, FUN = function(tr) {
    
    # Subset the fbt for the trait
    fbt_tr <- fbt[[tr]]
    
    # Use strsplit to parse
    tr_split <- strsplit(x = fbt_tr, split = sep)
    # Iterate over the list elements and make sure that n.obs are present. If not,
    # pad with NA
    tr_split1 <- lapply(X = tr_split, FUN = `length<-`, n.obs)
    # Convert to numeric
    tr_split2 <- lapply(X = tr_split1, FUN = as.numeric)
    
    # Bind rows
    tr_matrix <- do.call("rbind", tr_split2)
    
    # Calculate mean and sd
    tr_summary <- apply(X = tr_matrix, MARGIN = 1, 
                        FUN = function(row) c(mean = mean(row), sd = sd(row)))
    
    # Combine the split values and the summary
    tr_df <- as.data.frame(cbind(tr_matrix, t(tr_summary)))
    # Rename
    names(tr_df) <- c(paste(tr, paste("Obs", seq(n.obs), sep = ""), sep = "_"),
                      paste(tr, c("mean", "sd"), sep = "_"))
    
    # Return
    return(tr_df) })

  # Assemble new data.frame and return it
  bind_cols(fbt[,!names(fbt) %in% traits], bind_cols(parsed.data))

} # Close the function
