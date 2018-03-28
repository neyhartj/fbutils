#' Field Book Tables
#' 
#' @description Creates/checks data.frame objects for Field Book Table format.
#' 
#' @param fbt A Field Book Table object or an object to convert into a Field
#' Book Table object.
#' @param unique.id The column name of the Field Book data frame that corresponds
#' to the unique identification of each entry in the data table. Defaults to 'plot_id.'
#' @param row The column name of the Field Book data frame that corresponds to
#' the row of each entry in the data table. Defaults to 'row.'
#' @param column The column name of the Field Book data frame that corresponds to
#' the column of each entry in the data table. Defaults to 'column.'
#' @param line.name The column name of the Field Book data frame that corresponds to
#' the column of each entry name in the data table. Defaults to 'line.name.'
#' @param other.cols A character vector of other non-trait columns.
#' @param check.names Logical whether the column names should be checked for
#' proper formatting.
#' @param file The path to the Field Book Table .csv to be read in.
#' 
#' @details 
#' \code{as_field_book_table} converts data to a \code{data.frame} object with
#' formatting consistent with the Field Book Table.
#' 
#' 
#' A Field Book Table is a .csv file obtained by exporting data from the Field
#' Book app (\url{http://wheatgenetics.org/field-book}). See the app 
#' manual for instructions for exporting data. To export a Field Book Table file, 
#' one must check the "Table" format and the "All Important Columns" boxes when 
#' exporting. Data from a Field Book Table file will have at least three required
#' columns: a unique identifier, the row, and the column. Remaining columns can
#' include other variables.
#'
#' @return 
#' An object of class \code{data.frame} 
#' with formatting consistent with Field Book Table.
#' 
#' @import dplyr
#' @import stringr
#' 
#' @export
#' 
as_field_book_table <- function(fbt, unique.id = "plot_id", row = "row", 
                                column = "column", line.name = "line_name",
                                other.cols = NULL, check.names = TRUE) {
  
  ## Error handling

    # Make sure the column identifiers are characters or numeric
  if(!is.character(unique.id)) 
    stop("The 'unique.id' must be of type character.")
  if(!is.character(row)) 
    stop("The 'row' must be of type character.")
  if(!is.character(column)) 
    stop("The 'column' must be of type character.")
  if(!is.character(line.name)) 
    stop("The 'line.name' must be of type character.")
  
  # First remove any spaces in the column names
  new.names <- names(fbt) %>%
    str_replace_all(pattern = " ", replacement = "")
  
  names(fbt) <- new.names
  
  # Define replacements for special characters in the non traits
  non.trait.replacement.names <- c("-" = "_", "\\." = "_", " " = "_")
  # Different replacements for traits
  trait.replacement.names <- c("-" = "", "\\." = "", " " = "")
  
  # Set the other column names to lowercase and remove extra characters
  if (!is.null(other.cols)) {
    
    if (check.names) {
      names(other.cols) <- other.cols %>%
        str_to_lower() %>%
        str_replace_all(non.trait.replacement.names)
    } else {
      names(other.cols) <- other.cols
    }
  }
    
  # Renaming vector
  non.traits <- c("unique_id" = unique.id, "row" = row, "column" = column, 
                  "line_name" = line.name, other.cols)
  
  # Pull out the traits (i.e. not the non.traits) 
  traits <- setdiff(names(fbt), non.traits)
  
  # Set to title case and
  # remove special characters
  if (check.names) {
    names(traits) <- traits %>%
      str_to_title() %>%
      str_replace_all(trait.replacement.names)
  } else {
    names(traits) <- traits
  }
  
    
  if (any(!non.traits %in% names(fbt)))
    stop(str_c(c("The following non.traits were not found in the column names of the input x:", 
                  non.traits[!non.traits %in% colnames(fbt)]), collapse = "  "))
  
  ## Now proceed with fitting the field.book.table
  # First use dplyr to convert to tbl
  fbt1 <- tbl_df(fbt) %>%
    # Rename the identifier columns as "unique.id", "row", and "column"
    rename_(.dots = c(non.traits, traits))
  
  # Reset the non.trait and trait names
  non.traits <- names(non.traits)
  traits <- names(traits)
  
  # Reorder
  fbt2 <- fbt1 %>%
    select_(.dots = c(non.traits, traits))
  
  # Remove completely missing columns
  to.keep <- summarize_all(fbt2, funs(!all(is.na(.)))) %>%
    as.logical()
  
  fbt3 <- fbt2 %>%
    select(which(to.keep))
    
  # Return the data
  return(fbt3)
      
} # Close the function

#' Read in a Field Book Table
#' 
#' @description Reads in data from a Field Book Table as a \code{fbt} object.
#' 
#' @describeIn as_field_book_table
#' 
#' @import dplyr
#' @import stringr
#' 
#' @export
#' 
read_fb <- function(file, unique.id = "plot_id", row = "row", column = "column",
                    line.name = "line_name", other.cols = NULL, check.names = TRUE) {
  
  ## Error checking
  # Make sure the column identifiers are characters or numeric
  if(!is.character(unique.id)) 
    stop("The 'unique.id' must be of type character.")
  if(!is.character(row)) 
    stop("The 'row' must be of type character.")
  if(!is.character(column)) 
    stop("The 'column' must be of type character.")
  if(!is.character(line.name)) 
    stop("The 'line.name' must be of type character.")
  
  # Test read in data
  df <- read.csv(file = file, header = TRUE, as.is = TRUE)
  
  ## Convert to field.book.table
  fbt <- as_field_book_table(fbt = df, unique.id = unique.id, row = row, 
                             column = column, line.name = line.name,
                             other.cols = other.cols, check.names = check.names)
  
  
  # Return the tbl
  return(fbt)
  
} # Close the function
  
  
  
