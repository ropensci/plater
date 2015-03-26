#' Converts plate-formatted data to a data frame with one well per row.
#'
#' @param plateSize 
#' @param wellIdsColumn
#' @param filename
#' @param columnName
#' @return data frame
#' @export
read.plate <- function(plateSize, wellIdsColumn, filename, columnName) {
   
   if (length(filename) != length(columnName)) {
      stop(paste0("filename and columnName must have the same number of ",
         "elements, but filename had ", length(filename),
         " elements and columnName had ", length(columnName), " elements."))
   }
   
   data <- data.frame(w = getWellIds(plateSize))
   colnames(data) <- wellIdsColumn
   
   # get list of data frames with new columns
   result <- mapply(
      FUN = function(f, c) {
         getColumn(plateSize, wellIdsColumn, f, c)
      }, 
      filename, columnName)
   
   # combine result into one data frame
   result <- Reduce(function(x, y) merge(x, y, by = "wellIds", all = TRUE), 
      result)
   
   # only return rows which have value for more than the well ID
   nonNA <- apply(result, 1, FUN = function(x) sum(!is.na(x)))
   result[nonNA > 1, ]
}


getColumn <- function(plateSize, wellIdsColumn, filename, columnName) {
   
   # get data frame with annotations and remove unused wells
   annotations <- annotatePlate(filename, plateSize, columnName)
   annotations <- annotations[!(is.na(annotations[, columnName])), ]
   
   return(list(annotations))
}