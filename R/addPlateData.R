#' Adds a new column to a plate by well ID. 
#'  
#' Converts a .csv plate layout (with labels, conditions, data, etc.) to a 
#' column of a data frame and merges it into an existing data frame by well. 
#'
#' If data contains more wells than in filename, NA will be added to the merged
#' column for those wells. If the file contains more wells than data, an error
#' will result.
#'
#' @param data The data frame to merge the new annotations into. 
#' @param plateSize The number of wells on the plate
#' @param wellIdsColumn The name of the column in data containing the well IDs. 
#' @param filename The path of a .csv file formatted as described in 
#' \code{\link{annotatePlate}}.
#' @inheritParams annotatePlate 
#' @return Returns data with one new column containing the information in 
#' filename, merged by well ID. Empty wells are indicated with NA. 
#' @export
addPlateData <- function(data, plateSize, wellIdsColumn, filename, 
   columnName) {

   # validate well IDs
   missingLeadingZeroes <- areLeadingZeroesMissing(data, wellIdsColumn, 
      plateSize)
   
   # get data frame with annotations and remove unused wells
   annotations <- annotatePlate(filename, plateSize, columnName)
   annotations <- annotations[!(is.na(annotations[, columnName])), ]
   if(missingLeadingZeroes) {
      annotations$wellIds <- removeLeadingZeroes(annotations$wellIds)
   }
   
   # ensure data has all wells that the file does
   if(!(all(annotations$wellIds %in% data[ , wellIdsColumn]))) {
      stop(wrongWellsErrorMessage(data, wellIdsColumn, annotations))
   }

   result <- merge(data, annotations, by.x = wellIdsColumn, by.y = "wellIds", 
      all.x = TRUE) # all.x adds NA rows for wells missing from file
   
   # maintain order provided by user
   result <- result[order(match(
      result[ , wellIdsColumn], data[ , wellIdsColumn])), ]
   
   return(result)
}

#' Returns an error message indicating which wells in annotations are not in
#' data.
#'
#' Requires: at least one well in annotations$wellIds is not in 
#' data[[wellIdsColumn]]. 
#'
#' @param data The data frame missing some wells.
#' @param wellIdsColumn The name of the column in data containing the well IDs.
#' @param annotations The data frame with extra wells (with wellIdsColumn named
#' "wellIds")
#' @return An error message describing which wells are missing.
wrongWellsErrorMessage <- function(data, wellIdsColumn, annotations) {
   missing <- annotations$wellIds[!(annotations$wellIds %in% 
         data[ , wellIdsColumn])]
   if(length(missing) == 0) {
      stop("No wells are missing.")
   }
   missing <- paste0(missing, collapse = ", ")
   paste0("Some wells in your file are not in the data frame you ", 
      "provided, but they all should be. The missing wells are: ", 
      missing, ".")
}

#' Returns TRUE if leading zeroes are missing. 
#'
#' Stops if some leading zeroes are missing and others not or if invalid well IDs
#' are present. 
#'
#' @param data The data frame containing the well IDs. 
#' @param wellIdsColumn The name of the column containing the well IDs. 
#' @return TRUE if leading zeroes are missing or FALSE if all leading zeroes are
#' missing.  
areLeadingZeroesMissing <- function(data, wellIdsColumn, plateSize) {
   if ((all(data[ , wellIdsColumn] %in% getWellIds(plateSize)))) {
      return(FALSE)
   } else {
      if (!areLeadingZeroesValid(data, wellIdsColumn, plateSize)) {
         # leading zeroes are invalid
         if(!(all(data[, wellIdsColumn] %in% 
               getWellIdsWithoutLeadingZeroes(plateSize)))) {
            # some missing leading zeroes, some not, give up
            stop("Invalid well IDs--some have leading zeroes and some don't.")
         } else {
            return(TRUE)
         }
      } else {
         # problem is not with leading zeroes
         stop("Some well IDs are invalid.")
      }
   }
}