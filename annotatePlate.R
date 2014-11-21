annotate96WellPlate <- function(filename, columnName) {
   return (annotateNWellPlate(filename, columnName))
}

annotateNWellPlate <- function(filename, columnName) {
   plate <- readPlate(filename)
   
   validatePlate(plate)
   
   # remove column of row labels
   plate <- plate[-1]
   
   # validate file
   rows <- nrow(plate)
   cols <- ncol(plate)
   
   # convert to vector
   plate <- as.vector(t(plate))
   
   # generate well labels, with leading zeros for numbers below 10
   wells <- sapply(formatC(1:cols, width = 2, flag = "0"), 
      FUN = function(i) paste(LETTERS[1:rows], i, sep = ""))
   wells <- as.vector(t(wells))
   
   df <- data.frame(wellIds = wells, columnName = plate)
   
   return (df)   
}

readPlate <- function(filename) {
   read.table(filename, sep = ",", 
      skip = 1,  
      na.strings = "")
}

# requires:    plate is non-null
# throws:      stops if dimensions of plate (minus one column) are not (8, 12) 
#              or (16, 24) or if row labels are incorrect (not A:H or A:P)
validatePlate <- function(plate) {
   # correct dimensions
   rows <- nrow(plate)
   cols <- ncol(plate) - 1
   
   is96 <- rows == 8 && cols == 12
   is384 <- rows == 16 && cols == 24
   
   if(!(is96 || is384)) {
      stop(paste("Invalid plate dimensions. Found", rows, "rows and", 
         cols, "columns. Must be (8, 12) or (16, 24)."), call. = FALSE)
   }
   
   # correct row labels
   if(!identical(letters[1:8], tolower(plate[[1]])) &&
         !identical(letters[1:16], tolower(plate[[1]]))) {
      stop(wrongRowLabelsErrorMessage(plate))
   }
}

# requires:    plate is non-null and has valid dimensions, but the row labels 
#              are incorrect
# returns:     an error message, describing the row labels found and the row 
#              labels that were expected
wrongRowLabelsErrorMessage <- function(plate) {
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
