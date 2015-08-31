#' Returns a character vector of well IDs (e.g. A01..B10..H12) of length 12,  
#'              24, 48, 96, or 384 wells. 
#'
#' @param plateSize 12, 24, 48, 96, or 384 wells 
#' @return  A character vector of well IDs (e.g. A01..B05..H12) of length 12,  
#'              24, 48, 96, or 384 
#' @examples getWellIds(96)
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
   if (plateSize == 12) {
      return(4)
   } else if (plateSize == 24) {
      return(6)
   } else if (plateSize == 48) {
      return(8)
   } else if (plateSize == 96) {
      return(12)
   } else if (plateSize == 384) {
      return(24)
   } else {
      stop(paste0("Invalid plateSize: ", plateSize, 
         ". Must be 12, 24, 48, 96 or 384."))
   }
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