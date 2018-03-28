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
                 
  # Pull out the data on those traits
  trait.data <- fbt %>%
    select(which(names(.) %in% c("unique_id", "row", "column", "line_name", traits))) %>%
    gather(trait, value, -unique_id:-line_name)
  
  # Summarize
  trait.summary <- trait.data %>% 
    group_by(trait) %>% 
    summarize(mean = mean(value, na.rm = TRUE),
              sd = sd(value, na.rm = TRUE),
              n_NA = sum(is.na(value))) %>% 
    mutate(upper_bound = mean + (sd * max.sd), 
           lower_bound = mean - (sd * max.sd))
  
  # Join
  trait.data1 <- trait.data %>%
    full_join(trait.summary, by = "trait")
  
  # Detect outliers for each trait
  trait.outliers <- trait.data1 %>% 
    mutate(is_outlier = value < lower_bound | value > upper_bound)
  
  ## Put together an outlier summary
  outlier.summary <- trait.outliers %>% 
    filter(is_outlier) %>%
    select(unique_id:value)
  
  ## Trait summary
  trait.summary1 <- trait.summary %>%
    full_join(trait.outliers %>% 
                group_by(trait) %>% 
                summarize(n_outlier = sum(is_outlier, na.rm = TRUE)),
              by = "trait")

  # Print the outlier summary
  for (tr in traits) {
    
    # Subset the summary
    tr.summary <- trait.summary1 %>%
      filter(trait %in% tr)
    
    # Notify user
    cat("\n\nOutlier summary for trait: ", tr)
    cat("\nSample mean:", tr.summary$mean)
    cat("\nSample standard deviation: ", tr.summary$sd)
    cat("\nUpper bouund: ", tr.summary$upper_bound)
    cat("\nLower bound: ", tr.summary$lower_bound, "\n\n")
    
    # Print if outliers
    if (tr.summary$n_outlier > 0) {
      print(outlier.summary %>% filter(trait %in% tr) %>% as.data.frame())
      
    } else {
      cat("No outliers detected.")
    
    }
  }

  # Return the data
  return(outlier.summary)

} # Close the function
  
  