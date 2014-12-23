#' Annotates a 96-well plate. 
#'  
#' annotate96WellPlate() maps data from a microtiter plate layout to columns 
#' identified by well names. 
#'
#' @param filename The path of a .csv file formatted as described below.
#' @param columnName The name to give the data column on output. Default: "values"
#' @return Returns a two-column data frame, with one column called wellIds (A01, 
#'              A02..H12) and the other called columnName (containing the values 
#'              in the indicated wells). Empty wells are indicated with NA. 
#'              
#' @section File format:
#' The .csv file should be formatted as a microtiter plate. The top-left most 
#' cell is empty. The subsequent wells in the top row should be labeled 1-12. 
#' The subsequent cells in the first column should be labeled A-H. In other  
#' words:
#'
#' \tabular{ccccc}{
#'              \tab \strong{1} \tab \strong{2} \tab \strong{3} \tab \strong{...}\cr
#' \strong{A}   \tab A01        \tab A02        \tab A03        \tab ... \cr
#' \strong{B}   \tab B01        \tab B02        \tab B03        \tab ... \cr
#' \strong{...} \tab ...        \tab ...        \tab ...        \tab ... \cr
#' }
#' 
#' In this example, the cells within the plate contain the well IDs ("A01", 
#' "A02"), but they may contain arbitrary characters: numbers, letters, or 
#' punctuation, excepting the R comment character "#". Any cell may also be 
#' blank. 
annotate96WellPlate <- function(filename, columnName = "values") {
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
   
   df <- data.frame(wellIds = wells, columnName = plate, 
         stringsAsFactors = FALSE)
   names(df) <- c("wellIds", columnName)
   
   return (df)   
}

#' requires:    filename points to a valid .csv file, as specified above
#' returns:     a data frame created from the .csv file
readPlate <- function(filename) {
   read.table(filename, sep = ",", 
      skip = 1,  
      na.strings = "", stringsAsFactors = FALSE)
}

#' requires:    plate is non-null
#' param:       plate    a data frame
#' throws:      stops if dimensions of plate (minus one column) are not (8, 12) 
#'              or (16, 24) or if row labels are incorrect (not A:H or A:P)
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

#' requires:    plate is non-null and has at least one row and column
#' param:       plate    a data frame
#' returns:     true if dimensions of plate (minus one column) are not (8, 12) 
#'              or (16, 24)
arePlateDimensionsValid <- function(plate) {
   rows <- nrow(plate)
   cols <- ncol(plate) - 1
   
   is96 <- rows == 8 && cols == 12
   is384 <- rows == 16 && cols == 24
   
   return(is96 || is384)
}

#' requires:    plate is non-null and has at least 1 column
#' param:       plate    a data frame
#' returns:     true if column 1 is letters[1:8] or [1:16]. It may be in upper-, 
#'              lower-, or mixed-case.
areRowLabelsValid <- function(plate) {
   return(identical(letters[1:8], tolower(plate[[1]])) ||
         identical(letters[1:16], tolower(plate[[1]]))) 
}

#' requires:    plate is non-null and has valid dimensions, but the row labels 
#'              are incorrect
#' param:       plate    a data frame
#' returns:     an error message, describing the row labels found and the row 
#'              labels that were expected
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