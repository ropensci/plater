# Returns a character vector of well IDs (e.g. A01..B10..H12) of length 12,  
#              24, 48, 96, or 384 wells. 
#
# @param plate_size 12, 24, 48, 96, or 384 wells 
# @return  A character vector of well IDs (e.g. A01..B05..H12) of length 12,  
#              24, 48, 96, or 384 
# @examples get_well_ids(96)
get_well_ids <- function(plate_size) {
   cols <- number_of_columns(plate_size) # stops if not 12, 24, 48, 96, 384
   rows <- number_of_rows(plate_size)
   
   wells <- vapply(formatC(1:cols, width = 2, flag = "0"), 
      FUN = function(i) paste(LETTERS[1:rows], i, sep = ""), 
      FUN.VALUE = rep("character", rows))
   wells <- as.vector(t(wells))
   return(wells)
}

# Returns a character vector of well IDs without leading zeroes (e.g. A1..B10..
# H12) of length 12, 24, 48, 96, or 384 wells.
#
# @param plate_size 12, 24, 48, 96, or 384 wells 
# @return A character vector of well IDs without leading zeroes (e.g. A1..B10..
# H12) of length 12, 24, 48, 96, or 384 wells 
# @examples get_well_ids_without_leading_zeroes(96)
get_well_ids_without_leading_zeroes <- function(plate_size) {
   wells <- get_well_ids(plate_size)
   return(remove_leading_zeroes(wells))
}

# Returns wells with leading zeroes removed.
#
# @param wells A character vector of well IDs
# @return wells with leading zeroes removed (e.g. A1 rather than A01)
remove_leading_zeroes <- function(wells) {
   wells <- ifelse(substr(wells, 2, 2) == "0", 
      paste0(substr(wells, 1, 1), substr(wells, 3, 3)), 
      wells)
   return(wells)   
}

# Returns the number of rows in a plate of a given size. 
#
# @param plate_size 12, 24, 48, 96, or 384 wells 
# @return The number of rows in a plate of a given size. 
# @examples number_of_rows(96)
number_of_rows <- function(plate_size) {
   # stops if plate_size not 12, 24, 48, 96, or 384 wells 
   return(plate_size / number_of_columns(plate_size))
}

# Returns the number of columns in a plate of a given size.
#
# @param plate_size 12, 24, 48, 96, or 384 wells 
# @return The number of columns in a plate of a given size. 
# @examples number_of_columns(96)
number_of_columns <- function(plate_size) {
   n <- plate_dimensions("Columns", "PlateSize", plate_size)
   
   if (length(n) == 0) {
      stop(paste0("Invalid plate_size: ", plate_size, 
         ". Must be 12, 24, 48, 96 or 384."), call. = FALSE)
   }
   
   n
}

# Returns the size of a plate given the number of columns.
#
# @param columns 4, 6, 8, 12, 24
# @return the size of the plate or throws error if invalid number of columns. 
# @examples get_plate_size_from_number_of_columns(12)
get_plate_size_from_number_of_columns <- function(columns) {
   n <- plate_dimensions("PlateSize", "Columns", columns)
   
   if (length(n) == 0) {
      stop(paste0("Could not guess plate size from number of columns. ", 
                     "Invalid number of columns: ", columns))
   }
   
   n
}

# Helper function to return rows/columns/plate size from another value
#
# @param get The type of value to get ("Columns", "Rows", or "plate_size")
# @param from The type of value being provided ("Columns", "Rows", or "PlateSize")
# @param value The value
#
# @return the corresponding value, or an empty vector if invalid data supplied
plate_dimensions <- function(get, from, value) {
   dimensions <- data.frame(
              Columns    = c(4, 6, 8, 12, 24), 
               Rows      = c(3, 4, 6, 8, 16), 
              PlateSize  = c(12, 24, 48, 96, 384))
   
   which_row <- which(dimensions[, from] == value)
   
   dimensions[which_row, get]
}

# Throws an error if col_name is not a column in data.
# 
# @param data A data frame
# @param col_name The name of the column of well IDs
validate_column_is_in_data <- function(data, col_names) {
   if (any(!(col_names %in% colnames(data)))) {
      the_offender <- which(!(col_names %in% colnames(data)))
      the_offender <- col_names[the_offender]
      
      if (length(the_offender) > 1) {
         the_offender <- paste0(the_offender, collapse = ", ")
         the_offender <- paste0("are no columns named ", the_offender)
      } else {
         the_offender <- paste0("is no column named '", the_offender, "'")
      }
      
      stop(paste0("There ", the_offender, " in your data frame."), 
         call. = FALSE)
   }
}

# Guess the plate size from the column labels. 
# 
# Throws an error if the largest column label is not a valid number of columns
# for a standard plate size.
#
# @param A plate .csv file
# @return the size of the plate based on the column labels.
guess_plate_size <- function(file) {
   first_line <- readLines(file, n = 1)
   
   first_line_vector <- strsplit(first_line, ",")[[1]]
   
   # remove title field
   first_line_vector <- first_line_vector[-1]
   
   number_of_columns <- max(as.numeric(first_line_vector))
   
   get_plate_size_from_number_of_columns(number_of_columns)
}

# Check if the file exists
#
# Throws an error if the file doesn't exist or doesn't in with csv (regardless of
# capitalization).
#
check_file_path <- function(file) {
  if (is.null(file) || !file.exists(file)) {
    stop(paste0("Sorry, can't find your file '", file, "'."), call. = FALSE)
  }
  
  if (!(grepl("[Cc][Ss][Vv]$", file))) {
    stop(paste0("Sorry, '", file, "' doesn't have a proper CSV file extension."),
      call. = FALSE)
  }
}

# Check if well_ids_column is a valid string
#
# Throws an error if well_ids_column is null or an empty string
check_well_ids_column_name <- function(well_ids_column) {
  if (is.null(well_ids_column) || well_ids_column == "") {
    stop("Sorry, well_ids_column must not be NULL or an empty string.",
      call. = FALSE)
  }
}

# Check if the file is empty.
#
# Throws an error if the file is empty.
check_that_file_is_non_empty <- function(file) {
  if (length(readLines(file)) == 0) {
    stop(paste0("Sorry, '", file, "' is empty and must not be."), call. = FALSE)
  }
}