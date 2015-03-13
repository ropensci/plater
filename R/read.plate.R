#' @export
read.plate <- function(plateSize, wellIdsColumn, filename, columnName) {
   data <- data.frame(w = getWellIds(plateSize))
   colnames(data) <- wellIdsColumn
   addPlateData(data, plateSize, wellIdsColumn, filename, columnName)
}