#' Read plate layouts.
#' 
#' Converts data from a microtiter plate layout to a data frame with one well 
#' per row identified by well name.
#'
#' @param plate_size The number of wells in the plate
#' @param well_ids_column The name to give the column that will contain the well
#' names
#' @param file_names A character vector with the path(s) of one or more .csv files
#' formatted as described below.
#' @param column_names A character vector with the name(s) to give the column(s) 
#' corresponding to each .csv file, in the order the file names are listed. Must 
#' have as many elements as \code{file_names}.
#' @return Returns a data frame with each well as a row. One column will be 
#' named with \code{well_ids_column} and contain the well names (A01, A02..). 
#' There will be as many additional columns as elements in \code{file_names}, 
#' named with the corresponding elements in \code{column_names}, containing the
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
#' The parameters \code{file_names} and \code{column_names} take character vectors 
#' and represent the path to one or more .csv files containing the data to be 
#' read in and the name to give the corresponding column(s) in the resulting 
#' data frame. The two parameters need to have an equal number of arguments, 
#' i.e., one column name for each file. 
#' @export
read_plate <- function(plate_size, well_ids_column, file_names, column_names) {
   
   if (length(file_names) != length(column_names)) {
      stop(paste0("file_names and column_names must have the same number of ",
         "elements, but file_names had ", length(file_names),
         " elements and column_names had ", length(column_names), " elements."))
   }
   
   data <- data.frame(w = getWellIds(plate_size))
   colnames(data) <- well_ids_column
   
   # get list of data frames with new columns
   result <- mapply(
      FUN = function(f, c) {
         getColumn(plate_size, well_ids_column, f, c)
      }, 
      file_names, column_names)
   
   # combine result into one data frame
   result <- Reduce(function(x, y) merge(x, y, by = "wellIds", all = TRUE), 
      result)
   
   colnames(result)[colnames(result) == "wellIds"] <- well_ids_column
   
   # only return rows which have value for more than the well ID
   nonNA <- apply(result, 1, FUN = function(x) sum(!is.na(x)))
   result[nonNA > 1, ]
}


getColumn <- function(plate_size, well_ids_column, file_names, column_names) {
   
   # get data frame with annotations and remove unused wells
   annotations <- convertOnePlate(file_names, plate_size, column_names)
   annotations <- annotations[!(is.na(annotations[, column_names])), ]
   
   return(list(annotations))
}