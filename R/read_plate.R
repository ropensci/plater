#' Read plate layouts.
#' 
#' Converts data from a microtiter plate layout to a data frame with one well 
#' per row identified by well name.
#'
#' @param file A character vector with the path of a .csv file formatted as 
#' described below.
#' @param well_ids_column The name to give the column that will contain the well
#' names. Default "Wells".
#' @param plate_size The number of wells in the plate. Must be 12, 24, 48, 96 or
#'  384. Default 96.
#' @return Returns a data frame with each well as a row. One column will be 
#' named with \code{well_ids_column} and contain the well names (A01, A02..). 
#' There will be as many additional columns as plates in \code{file}. Empty 
#' wells are indicated with NA.
#' 
#' @section File format:
#' The .csv file should be formatted as a microtiter plate. The top-left most 
#' cell contains the name to use for the column representing that plate. For 
#' example, for a 96-well plate, the subsequent wells in the top row should be 
#' labeled 1-12. The subsequent cells in the first column should be labeled A-H. 
#' That is:
#'
#' \tabular{ccccc}{
#' ColName      \tab \strong{1} \tab \strong{2} \tab \strong{3} \tab \strong{...}\cr
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
#' @section Multiple columns: 
#' Multiple columns of information about a plate can be included in a single 
#' file. After the first plate, leave one row blank, and then add another plate
#' formatted as described above. As many plates as necessary can be included in
#' a single file (e.g. data measured, subject, treatment, replicate, etc.).
#'
#' @export
read_plate <- function(file, well_ids_column = "Wells", plate_size = 96) {
  
   data <- data.frame(w = getWellIds(plate_size))
   colnames(data) <- well_ids_column
   
   # read in file
   raw_file <- readLines(file)
   
   # get list of data frames with new columns
   number_of_rows <- numberOfRows(plate_size)
   
   number_of_plates <- calculateNumberOfPlates(raw_file, number_of_rows)
   
   raw_file_list <- lapply(1:number_of_plates, FUN =
      function(plate) {
         first_row <- (plate - 1) * (number_of_rows + 1) + plate
         last_row <- first_row + number_of_rows
         raw_file[first_row:last_row]
      }
   )
   
   result <- lapply(raw_file_list, FUN = function(f) {
         getColumn(plate_size, f)
      }
   )
   
   if (length(result) == 1) {
      result <- result[[1]]
   } else {
      # ensure that plate names are unique
      result <- checkUniquePlateNames(result)

      # combine result into one data frame
      result <- Reduce(function(x, y) merge(x, y, by = "wellIds", all = TRUE), 
         result)      
   }
   
   colnames(result)[colnames(result) == "wellIds"] <- well_ids_column
   
   # only return rows which have value for more than the well ID
   nonNA <- apply(result, 1, FUN = function(x) sum(!is.na(x)))
   result[nonNA > 1, ]
}


# Gets one column from a plate and removes any NA rows.
#
# @param plate_size The number of wells in the plate
# @param plate A character vector with each element containing a comma-
# delimited row of a plate
#
# @return A two-column data frame, one column containing well IDs and the other
# containing the data contained in `plate`.
getColumn <- function(plate_size, plate) {
   # get data frame with annotations and remove unused wells
   annotations <- convertOnePlate(plate, plate_size)
   
   # remove any NA values from the new column
   column <- colnames(annotations)[colnames(annotations) != "wellIds"]
   annotations <- annotations[!(is.na(annotations[, column])), ]
   
   return(annotations)
}

# Calculate the number of plates contained in the file.
#
# Throws an error if the number of elements in `raw_file` cannot be parsed to
# an integer number of plates.
#
# @param raw_file A text vector with each element containing a comma-delimited
# row
# @param number_of_rows The expected number of rows for the given plate size
#
# @return the number of plates in the file
calculateNumberOfPlates <- function(raw_file, number_of_rows) {
   result <- (length(raw_file) + 1) / (number_of_rows + 2)
   
   is_integer <- function(x) x %% 1 == 0
   if (is_integer(result)) {
      return(result)
   } else {
      # try not adding one (in case a blank line ends raw_file)
      result <- (length(raw_file)) / (number_of_rows + 2)
      if (is_integer(result)) {
         return(result)
      } else {
         stop(paste0("File length is incorrect. Must be a multiple of the ", 
                     "number of rows in the plate plus a header row for each ",
                     "plate and a blank row between plates."))
      }
   }
}

# Make sure all plate names are unique. 
# 
# If each plate (column in the final data frame) doesn't have a unique name,
# append ".#" where "#" is the column's number, to the name. 
#
# @param result list of two-column data frames, where the names of the second
# columns will be checked for duplication.
#
# @return result with any duplicated names replaced
checkUniquePlateNames <- function(result) {
   # get plate names
   plateNames <- sapply(result, FUN = function(x) colnames(x)[2])
   
   if(any(duplicated(plateNames))) {
      duplicates <- which(duplicated(plateNames))
      
      # replace duplicate column names with .n 
      result <- lapply(1:length(result), FUN = function(n) {
         if (n %in% duplicates) {
            newName <- paste0(colnames(result[[n]])[2], ".", n)
            colnames(result[[n]])[2] <- newName
            result[[n]]
         } else {
            result[[n]]
         }
      })
   }
   return(result)
}