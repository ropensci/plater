#' Adds a new column to a 96-well plate by well ID. 
#'  
#' Converts a .csv plate layout (with labels, conditions, data, etc.) to a 
#' column of a data frame and merges it into an existing data frame by well. 
#'
#' @param data The data frame to merge the new annotations into. 
#' @param wellIdsColumn The name of the column in data containing the well IDs. 
#' @param filename The path of a .csv file formatted as described in 
#' \code{\link{annotate96WellPlate}}.
#' @param columnName The name to give the new column on output. Default: "values"
#' @return Returns data with one new column containing the information in 
#' filename, merged by well ID. Empty wells are indicated with NA. 
annotateAndMerge96WellPlate <- function(data, wellIdsColumn, filename, columnName) {
   # validate that data$wellIdsColumn is in the right format
   # what if fewer than 96 wells?
   # what if missing leading 0s?
   # call annotate96WellPlate(filename, columnName) and save result
   # merge result into data by wellIdsColumn
   
   # TODO
   
}