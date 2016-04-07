# Returns a character vector of well IDs (e.g. A01..B10..H12) of length 12,  
#              24, 48, 96, or 384 wells. 
#
# @param plateSize 12, 24, 48, 96, or 384 wells 
# @return  A character vector of well IDs (e.g. A01..B05..H12) of length 12,  
#              24, 48, 96, or 384 
# @examples getWellIds(96)
getWellIds <- function(plateSize) {
   cols <- numberOfColumns(plateSize) # stops if not 12, 24, 48, 96, 384
   rows <- numberOfRows(plateSize)
   
   wells <- sapply(formatC(1:cols, width = 2, flag = "0"), 
      FUN = function(i) paste(LETTERS[1:rows], i, sep = ""))
   wells <- as.vector(t(wells))
   return(wells)
}

# Returns a character vector of well IDs without leading zeroes (e.g. A1..B10..
# H12) of length 12, 24, 48, 96, or 384 wells.
#
# @param plateSize 12, 24, 48, 96, or 384 wells 
# @return A character vector of well IDs without leading zeroes (e.g. A1..B10..
# H12) of length 12, 24, 48, 96, or 384 wells 
# @examples getWellIdsWithoutLeadingZeroes(96)
getWellIdsWithoutLeadingZeroes <- function(plateSize) {
   wells <- getWellIds(plateSize)
   return(removeLeadingZeroes(wells))
}

# Returns wells with leading zeroes removed.
#
# @param wells A character vector of well IDs
# @return wells with leading zeroes removed (e.g. A1 rather than A01)
removeLeadingZeroes <- function(wells) {
   wells <- ifelse(substr(wells, 2, 2) == "0", 
      paste0(substr(wells, 1, 1), substr(wells, 3, 3)), 
      wells)
   return(wells)   
}

# Returns the number of rows in a plate of a given size. 
#
# @param plateSize 12, 24, 48, 96, or 384 wells 
# @return The number of rows in a plate of a given size. 
# @examples numberOfRows(96)
numberOfRows <- function(plateSize) {
   # stops if plateSize not 12, 24, 48, 96, or 384 wells 
   return(plateSize / numberOfColumns(plateSize))
}

# Returns the number of columns in a plate of a given size.
#
# @param plateSize 12, 24, 48, 96, or 384 wells 
# @return The number of columns in a plate of a given size. 
# @examples numberOfColumns(96)
numberOfColumns <- function(plateSize) {
   n <- plate_dimensions("Columns", "PlateSize", plateSize)
   
   if (length(n) == 0) {
      stop(paste0("Invalid plateSize: ", plateSize, 
         ". Must be 12, 24, 48, 96 or 384."), call. = FALSE)
   }
   
   n
}

# Returns the size of a plate given the number of columns.
#
# @param columns 4, 6, 8, 12, 24
# @return TRUE and the size of the plate or FALSE and an error message. 
# @examples get_plate_size_from_number_of_columns(12)
get_plate_size_from_number_of_columns <- function(columns) {
   n <- plate_dimensions("PlateSize", "Columns", columns)
   
   if (length(n) == 0) {
      return(paste0("Invalid number of columns: ", columns))
   }
   
   n

}

# Helper function to return rows/columns/plate size from another value
#
# @param get The type of value to get ("Columns", "Rows", or "PlateSize")
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

# Throws an error if wellIdsColumn is not a column in data.
# 
# @param data A data frame
# @param wellIdsColumn The name of the column of well IDs
validateColumnIsInData <- function(data, colName) {
   if (!(colName %in% colnames(data))) {
      stop(paste0("There is no column named '", colName, 
         "' in your data frame."), call. = FALSE)
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
   first_line_vector <- first_line_vector[2:length(first_line_vector)]
   
   number_of_columns <- max(as.numeric(first_line_vector))
   
   size <- get_plate_size_from_number_of_columns(number_of_columns)
   
   if(is.numeric(size)) {
      return(size)
   } else {
      stop(paste0("Could not guess plate size from number of columns. ", size), 
         call. = FALSE)
   }
}