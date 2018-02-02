# Returns a character vector of well IDs (e.g. A01..B10..H12) of length 6, 12,  
#              24, 48, 96, 384, or 1536 wells. 
#
# @param plate_size 6, 12, 24, 48, 96, 384, or 1536 wells 
# @return  A character vector of well IDs (e.g. A01..B05..H12) of length 6, 12,  
#              24, 48, 96, 384, or 1536 
# @examples get_well_ids(96)
get_well_ids <- function(plate_size) {
   cols <- number_of_columns(plate_size) # stops if not 12, 24, 48, 96, 384
   rows <- number_of_rows(plate_size)
   
   wells <- vapply(formatC(1:cols, width = 2, flag = "0"), 
      FUN = function(i) paste(MEGALETTERS(1:rows), i, sep = ""), 
      FUN.VALUE = rep("character", rows))
   wells <- as.vector(t(wells))
   return(wells)
}

# 1536-well plate includes row names that are double letters
# e.g. AB, so can't just use LETTERS
MEGALETTERS <- function(x) c(LETTERS[1:26], paste0("A", LETTERS[1:26]))[x]

# Returns a character vector of well IDs without leading zeroes (e.g. A1..B10..
# H12) of length 6, 12, 24, 48, 96, 384, or 1536 wells.
#
# @param plate_size 6, 12, 24, 48, 96, 384, or 1536 wells 
# @return A character vector of well IDs without leading zeroes (e.g. A1..B10..
# H12) of length 6, 12, 24, 48, 96, 384, or 1536 wells 
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
   # if it matches a 0 that is not the last character, remove all 0s from that 
   # ID (can't have more than one 0)
   # just str_replace doesn't work on the first regex because it also replaces
   # the following character
   wells <- ifelse(grepl("0[^$]", wells), 
      gsub("0", "", wells), wells)
   return(wells)   
}

# Returns the number of rows in a plate of a given size. 
#
# @param plate_size 6, 12, 24, 48, 96, 384, or 1536 wells 
# @return The number of rows in a plate of a given size. 
# @examples number_of_rows(96)
number_of_rows <- function(plate_size) {
   # stops if plate_size not 6, 12, 24, 48, 96, 384, or 1536 wells 
   return(plate_size / number_of_columns(plate_size))
}

# Returns the number of columns in a plate of a given size.
#
# @param plate_size 6, 12, 24, 48, 96, 384 or 1536 wells 
# @return The number of columns in a plate of a given size. 
# @examples number_of_columns(96)
number_of_columns <- function(plate_size) {
   n <- plate_dimensions("Columns", "PlateSize", plate_size)
   
   if (length(n) == 0) {
      stop(paste0("Invalid plate_size: ", plate_size, 
         ". Must be 6, 12, 24, 48, 96, 384, or 1536."), call. = FALSE)
   }
   
   n
}

# Returns the size of a plate given the number of columns.
#
# @param columns 3, 4, 6, 8, 12, 24, 48
# @return the size of the plate or throws error if invalid number of columns. 
# @examples get_plate_size_from_number_of_columns(12)
get_plate_size_from_number_of_columns <- function(columns) {
   n <- plate_dimensions("PlateSize", "Columns", columns)
   
   if (length(n) == 0) {
      stop(paste0("Could not guess plate size from number of columns. ", 
                     "Invalid number of columns: ", columns), call. = FALSE)
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
              Columns    = c(3, 4,  6,  8,  12, 24,  48), 
               Rows      = c(2, 3,  4,  6,  8,  16,  32), 
              PlateSize  = c(6, 12, 24, 48, 96, 384, 1536))
   
   which_row <- which(dimensions[[from]] == value)
   
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
   first_line <- read_lines(file, n = 1)
   
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
  if (length(read_lines(file)) == 0) {
    stop(paste0("Sorry, '", file, "' is empty and must not be."), call. = FALSE)
  }
}

# Check if more than one file is provided
#
# Throws an error if multiple files are provided
check_that_only_one_file_is_provided <- function(file) {
  if (length(file) > 1) {
    stop(paste0("Sorry, only one file should be provided, but you provided ", 
                "multiple. Maybe you wanted read_plates()?"), call. = FALSE)
  }
}

# Wrapper around base::readLines
# Doesn't warn if there's an incomplete final line for the file
# See github issue 17, where layouts created on Mac created warning
# https://github.com/ropenscilabs/plater/issues/17
read_lines <- function(file, n = -1L) {
    readLines(file, n = n, warn = FALSE)
}

# returns data sorted by well ID
# handles 1536-well plates correctly (i.e. AA01 coming after Z48 not B48)
sort_by_well_ids <- function(data, well_ids_column, plate_size) {
   data[order(match(data[[well_ids_column]], get_well_ids(plate_size))), , drop = FALSE]
}