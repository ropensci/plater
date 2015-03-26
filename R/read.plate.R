#' @export
read.plate <- function(plateSize, wellIdsColumn, filename, columnName) {
   data <- data.frame(w = getWellIds(plateSize))
   colnames(data) <- wellIdsColumn
   result <- addPlate(data, plateSize, wellIdsColumn, filename, columnName)
   
   # only return rows which have value for more than the well ID
   nonNA <- apply(result, 1, FUN = function(x) sum(!is.na(x)))
   result[nonNA > 1, ]
}