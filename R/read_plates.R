#' Read multiple plateR-formatted files and combine result into one data frame.
#' 
#' A wrapper around \code{read_plate} that handles multiple plates and combines
#' them all into a single data frame.
#' 
#' @param files A character vector with the paths of one or more plateR-formatted
#' .csv files.
#' @param plate_names A character vector the same length as \code{files} with the
#' names to give the individual plates in the resulting data frame. Defaults to
#' the file names (stripped of path and .csv). 
#' @inheritParams read_plate  
#' @return Returns a data frame like that returned by \code{read_plate}, 
#' containing the data from all of the plates stacked on top of each other. The
#' plates will be identified with a column called "Plate" containing the names
#' given in \code{plate_names}. 
#' 
#' @export
read_plates <- function(files, plate_names = NULL, well_ids_column = "Wells") {
   if(is.null(plate_names)) {
      plate_names <- generate_plate_names(files)
   }
   
   if(length(files) != length(plate_names)) {
      stop("files and plate_names must have the same length.")
   }
   
   list_of_data_frames <- mapply(
      FUN = function(file, plate_name) { 
         p <- read_plate(file, well_ids_column)
         p$Plate <- plate_name
         p
      }, 
      files, plate_names, SIMPLIFY = FALSE
   )
   
   result <- dplyr::bind_rows(list_of_data_frames)

   rownames(result) <- NULL
   
   result
}

# Strip file paths and ".csv" to generate just file name as plate identifier
generate_plate_names <- function(files) {
   # remove leading file paths
   files <- regmatches(files, regexpr("[^/\\\\]*.[Cc][Ss][Vv]$", files))
   
   # remove .csv
   gsub(".[Cc][Ss][Vv]$", "", files)
}




