#' Read a plate layouts.
#' 
#' Converts data from a microtiter plate layout to a data frame with one well 
#' per row identified by well name.
#'
#' @param plateSize The number of wells in the plate
#' @param wellIdsColumn The name to give the column that will contain the well
#' names
#' @param fileNames A character vector with the path(s) of one or more .csv files
#' formatted as described below.
#' @param columnNames A character vector with the name(s) to give the column(s) 
#' corresponding to each .csv file, in the order the file names are listed. Must 
#' have as many elements as \code{fileNames}.
#' @return Returns a data frame with each well as a row. One column will be 
#' named with \code{wellIdsColumn} and contain the well names (A01, A02..). 
#' There will be as many additional columns as elements in \code{fileNames}, 
#' named with the corresponding elements in \code{columnNames}, containing the
#' data from the relevant wells. Empty wells are indicated with NA.
#' 
#' @section File format:
#' The .csv file should be formatted as a microtiter plate. The top-left most 
#' cell is empty. For example, for a 96-well plate, the subsequent wells in the 
#' top row should be labeled 1-12. The subsequent cells in the first column 
#' should be labeled A-H. That is:
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
#' 
#' Note that Microsoft Excel will sometimes include cells that appear to be 
#' blank in the .csv files it produces, so the files may have spurious columns
#' or rows outside of the plate, causing errors. To solve this problem, copy and
#' paste just the cells within the plate to a fresh worksheet and save it.
#' 
#' @section Multiple files: 
#' The parameters \code{fileNames} and \code{columnNames} take character vectors 
#' and represent the path to one or more .csv files containing the data to be 
#' read in and the name to give the corresponding column(s) in the resulting 
#' data frame. The two parameters need to have an equal number of arguments, 
#' i.e., one column name for each file. 
#' @export
read.plate <- function(plateSize, wellIdsColumn, fileNames, columnNames) {
   
   if (length(fileNames) != length(columnNames)) {
      stop(paste0("fileNames and columnNames must have the same number of ",
         "elements, but fileNames had ", length(fileNames),
         " elements and columnNames had ", length(columnNames), " elements."))
   }
   
   data <- data.frame(w = getWellIds(plateSize))
   colnames(data) <- wellIdsColumn
   
   # get list of data frames with new columns
   result <- mapply(
      FUN = function(f, c) {
         getColumn(plateSize, wellIdsColumn, f, c)
      }, 
      fileNames, columnNames)
   
   # combine result into one data frame
   result <- Reduce(function(x, y) merge(x, y, by = "wellIds", all = TRUE), 
      result)
   
   colnames(result)[colnames(result) == "wellIds"] <- wellIdsColumn
   
   # only return rows which have value for more than the well ID
   nonNA <- apply(result, 1, FUN = function(x) sum(!is.na(x)))
   result[nonNA > 1, ]
}


getColumn <- function(plateSize, wellIdsColumn, fileNames, columnNames) {
   
   # get data frame with annotations and remove unused wells
   annotations <- convertOnePlate(fileNames, plateSize, columnNames)
   annotations <- annotations[!(is.na(annotations[, columnNames])), ]
   
   return(list(annotations))
}