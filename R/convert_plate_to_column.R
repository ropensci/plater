# Reads and converts one plate to a data frame. 
#  
# convertOnePlate() maps data from a microtiter plate layout to columns 
# identified by well names. Columns are named by the value held in the top-left
# cell of \code{plate} or called "value" if empty.
#
# @param plate A character vector with each element containing a comma-
# delimited row of a plate
# @param plateSize The number of wells in the plate
# @return Returns a two-column data frame, with one column called wellIds (A01, 
#              A02..) and the other containing the values in the indicated 
#              wells). Empty wells are omitted.  
convert_plate_to_column <- function(plate, plateSize) {
   plate <- readPlate(plate)
   
   columnName <- plate[1, 1]
   
   if(is.na(columnName)) {
      columnName <- "values"
   }
   
   # remove column names
   plate <- plate[-1, ]
   
   # stop if plate is invalid
   validatePlate(plate, plateSize)
   
   # remove column of row labels
   plate <- plate[-1]
   
   # get plate dimensions
   rows <- nrow(plate)
   cols <- ncol(plate)
   
   # convert to vector
   plate <- as.vector(t(plate))
   
   # generate well labels,
   wells <- getWellIds(rows * cols)
   
   df <- data.frame(wellIds = wells, columnName = plate, 
         stringsAsFactors = FALSE)
   names(df) <- c("wellIds", columnName)
   
   # remove any NA values from the new column
   column <- colnames(df)[colnames(df) != "wellIds"]
   df <- df[!(is.na(df[, column])), ]
   
   return (df)   
}

# requires:    plate contains a character vector, as specified above
# returns:     a data frame created from the plate
readPlate <- function(plate) {
   utils::read.table(textConnection(plate), sep = ",", 
      na.strings = "", stringsAsFactors = FALSE)
}

# requires:    plate is non-null
# param:       plate    a data frame
# param:       plateSize expected plate size   
# throws:      stops if dimensions of plate (minus one column) are not (8, 12) 
#              or (16, 24) or if row labels are incorrect (not A:H or A:P)
validatePlate <- function(plate, plateSize) {
   if (!arePlateDimensionsValid(plate, plateSize)) {
      stop(paste0("Invalid plate dimensions. Found ", nrow(plate), " rows and ", 
         ncol(plate) - 1, " columns. Must be (", numberOfRows(plateSize), ", ",
         numberOfColumns(plateSize), ") for a ", plateSize, "-well plate."), 
         call. = FALSE)
   }
      
   if (!areRowLabelsValid(plate, plateSize)) {
      stop(wrongRowLabelsErrorMessage(plate, plateSize))
   }
}

# requires:    plate is non-null and has at least one row and column
# param:       plate    a data frame
# param:       plateSize expected plate size   
# returns:     true if dimensions of plate (minus one column) are not (8, 12) 
#              or (16, 24)
arePlateDimensionsValid <- function(plate, plateSize) {
   rows <- nrow(plate)
   cols <- ncol(plate) - 1
   
   expectedRows <- numberOfRows(plateSize)
   expectedCols <- numberOfColumns(plateSize)
   
   return(rows == expectedRows && cols == expectedCols)
}

# requires:    plate is non-null and has at least 1 column
# param:       plate    a data frame
# param:       plateSize expected plate size   
# returns:     true if column 1 is letters[1:8] or [1:16]. It may be in upper-, 
#              lower-, or mixed-case.
areRowLabelsValid <- function(plate, plateSize) {
   rows <- numberOfRows(plateSize)
   
   rowLabels <- trimWhiteSpace(tolower(plate[[1]]))
   
   return(identical(letters[1:rows], rowLabels)) 
}

# requires:    plate is non-null and has valid dimensions, but the row labels 
#              are incorrect
# param:       plate    a data frame
# param:       plateSize expected plate size
# returns:     an error message, describing the row labels found and the row 
#              labels that were expected
wrongRowLabelsErrorMessage <- function(plate, plateSize) {
   if (!arePlateDimensionsValid(plate, plateSize)) {
      stop("plate must have valid dimensions", call. = FALSE)
   }
   if (areRowLabelsValid(plate, plateSize)) {
      stop("row labels must be invalid", call. = FALSE)
   }
   found <- paste(plate[[1]], collapse = " ")
   
   rows <- numberOfRows(plateSize)
   
   lower <- paste(letters[1:rows], collapse = " ")
   upper <- paste(LETTERS[1:rows], collapse = " ")
   
   output <- paste("Correct row labels not found. Found '", found, 
      "' but expected '",lower, "' or '", upper, "'.", sep = "")
   
   return(output)
}

trimWhiteSpace <- function(text) {
   whiteSpace <- "^\\s+|\\s+$"
   
   gsub(whiteSpace, replacement = "", text)
}