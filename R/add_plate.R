#' Reads plate layouts and adds them to an existing data frame. 
#'  
#' Converts data from a microtiter plate layout to a data frame with one well 
#' per row and merges it into an existing data frame by well name. 
#'
#' If data contains more wells than in \code{file}, NA will be added to the 
#' merged column for those wells. If the file contains more wells than 
#' \code{data}, an error will result.
#'
#' @param file A character vector with the path of a .csv file formatted as 
#' as described in \code{\link{read_plate}}.
#' @param data The data frame to merge the new annotations into. Must contain
#' a column with well names.
#' @param well_ids_column The name of the column in \code{data} containing the 
#' well IDs. 
#' @inheritParams read_plate 
#' @return Returns data with as many new columns as plates in \code{file}. 
#' Empty wells are indicated with NA.
#' @export
add_plate <- function(file, data, well_ids_column, plate_size = 96) {
   toAdd <- read_plate(file, "wellIds", plate_size)

   # validate well_ids_column
   validateColumnIsInData(data, well_ids_column)
   
   # validate well IDs
   missingLeadingZeroes <- areLeadingZeroesMissing(data, well_ids_column, 
      plate_size)   
   
   if(missingLeadingZeroes) {
      toAdd$wellIds <- removeLeadingZeroes(toAdd$wellIds)
   }
   
   # ensure data has all wells that the file does
   if(!(all(toAdd$wellIds %in% data[ , well_ids_column]))) {
      stop(wrongWellsErrorMessage(data, well_ids_column, toAdd))
   }
   
   # merge new columns with input data frame
   result <- merge(data, toAdd, by.x = well_ids_column, by.y = "wellIds", 
      all.x = TRUE) # all.x adds NA rows for wells missing from file
   
   # maintain order provided by user
   result <- result[order(
      match(
         result[ , well_ids_column], 
         data[ , well_ids_column]
      )
   ), ]
   
}

# Returns an error message indicating which wells in annotations are not in
# data.
#
# Requires: at least one well in annotations$wellIds is not in 
# data[[well_ids_column]]. 
#
# @param data The data frame missing some wells.
# @param well_ids_column The name of the column in data containing the well IDs.
# @param annotations The data frame with extra wells (with well_ids_column named
# "wellIds")
# @return An error message describing which wells are missing.
wrongWellsErrorMessage <- function(data, well_ids_column, annotations) {
   missing <- annotations$wellIds[!(annotations$wellIds %in% 
         data[ , well_ids_column])]
   if(length(missing) == 0) {
      stop("No wells are missing.")
   }
   missing <- paste0(missing, collapse = ", ")
   paste0("Some wells in your file are not in the data frame you ", 
      "provided, but they all should be. The missing wells are: ", 
      missing, ".")
}

# Returns TRUE if leading zeroes are missing. 
#
# Stops if some leading zeroes are missing and others not or if invalid well IDs
# are present. 
#
# @param data The data frame containing the well IDs. 
# @param well_ids_column The name of the column containing the well IDs. 
# @return TRUE if leading zeroes are missing or FALSE if all leading zeroes are
# missing.  
areLeadingZeroesMissing <- function(data, well_ids_column, plate_size) {
   if ((all(data[ , well_ids_column] %in% getWellIds(plate_size)))) {
      return(FALSE)
   } else {
      if (!areLeadingZeroesValid(data, well_ids_column, plate_size)) {
         # leading zeroes are invalid
         if(!(all(data[, well_ids_column] %in% 
               getWellIdsWithoutLeadingZeroes(plate_size)))) {
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