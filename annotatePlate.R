

# annotate96WellPlate() and annotate384WellPlate() map data from a microtiter
# plate layout to columns identified by well names. 
#
# The functions take a .csv file formatted as a microtiter plate and return a 
# data frame with two columns: one with wellIds (A01, A02..H12) and one with the
# contents of each well within the plate. 
#
# The file must be formatted as follows...


### add test for blank columns


################################################################################
annotate96WellPlate <- function(filename, columnName) {
   return (annotateNWellPlate(filename, columnName))
}

annotateNWellPlate <- function(filename, columnName) {
   plate <- readPlate(filename)
   
   # stop if plate is invalid
   validatePlate(plate)
   
   # remove column of row labels
   plate <- plate[-1]
   
   # get plate dimensions
   rows <- nrow(plate)
   cols <- ncol(plate)
   
   # convert to vector
   plate <- as.vector(t(plate))
   
   # generate well labels,
   wells <- getWellIds(rows * cols)
   
   df <- data.frame(wellIds = wells, columnName = plate)
   names(df) <- c("wellIds", columnName)
   
   return (df)   
}

# requires:    plateSize == 96 or 384
# returns:     a character vector of well IDs (e.g. A01..B05..H12) of length 96 
#              or 384 
getWellIds <- function(plateSize) {
   if(plateSize == 96) {
      cols = 12
   } else if (plateSize == 384) {
      cols = 24
   } else {
      stop(paste0("Invalid plateSize: ", plateSize, ". Must be 96 or 384."))
   }
   rows = plateSize / cols
   
   wells <- sapply(formatC(1:cols, width = 2, flag = "0"), 
      FUN = function(i) paste(LETTERS[1:rows], i, sep = ""))
   wells <- as.vector(t(wells))
   return(wells)
}

# requires:    filename points to a valid .csv file, as specified above
# returns:     a data frame created from the .csv file
readPlate <- function(filename) {
   read.table(filename, sep = ",", 
      skip = 1,  
      na.strings = "")
}

# requires:    plate is non-null
# param:       plate    a data frame
# throws:      stops if dimensions of plate (minus one column) are not (8, 12) 
#              or (16, 24) or if row labels are incorrect (not A:H or A:P)
validatePlate <- function(plate) {
   if (!arePlateDimensionsValid(plate)) {
      stop(paste("Invalid plate dimensions. Found", nrow(plate), "rows and", 
         ncol(plate) - 1, "columns. Must be (8, 12) or (16, 24)."), 
         call. = FALSE)
   }
      
   if (!areRowLabelsValid(plate)) {
      stop(wrongRowLabelsErrorMessage(plate))
   }
}

# requires:    plate is non-null and has at least one row and column
# param:       plate    a data frame
# returns:     true if dimensions of plate (minus one column) are not (8, 12) 
#              or (16, 24)
arePlateDimensionsValid <- function(plate) {
   rows <- nrow(plate)
   cols <- ncol(plate) - 1
   
   is96 <- rows == 8 && cols == 12
   is384 <- rows == 16 && cols == 24
   
   return(is96 || is384)
}

# requires:    plate is non-null and has at least 1 column
# param:       plate    a data frame
# returns:     true if column 1 is letters[1:8] or [1:16]. It may be in upper-, 
#              lower-, or mixed-case.
areRowLabelsValid <- function(plate) {
   return(identical(letters[1:8], tolower(plate[[1]])) ||
         identical(letters[1:16], tolower(plate[[1]]))) 
}

# requires:    plate is non-null and has valid dimensions, but the row labels 
#              are incorrect
# param:       plate    a data frame
# returns:     an error message, describing the row labels found and the row 
#              labels that were expected
wrongRowLabelsErrorMessage <- function(plate) {
   if (!arePlateDimensionsValid(plate)) {
      stop("plate must have valid dimensions")
   }
   if (areRowLabelsValid(plate)) {
      stop("row labels must be invalid")
   }
   found <- paste(plate[[1]], collapse = " ")
   if(length(plate[[1]]) < 9) {
      rows <- 8
   } else {
      rows <- 16
   }
   lower <- paste(letters[1:rows], collapse = " ")
   upper <- paste(LETTERS[1:rows], collapse = " ")
   
   output <- paste("Correct row labels not found. Found '", found, 
      "' but expected '",lower, "' or '", upper, "'.", sep = "")
   
   return(output)
}