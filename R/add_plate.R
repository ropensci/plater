#' Read a plater-formatted file and combine it with an existing data frame. 
#' 
#' Converts data from \code{plater} format to a data frame with one well 
#' per row and merges it into an existing data frame by well name. 
#'
#' If data contains more wells than in \code{file}, NA will be added to the 
#' merged column(s) for those wells. If the file contains more wells than 
#' \code{data}, those wells will be added to the bottom of the result with NA
#' for the columns in \code{data}.
#'
#' @param data The data frame to merge the file into. Must contain a column with
#' well names.
#' @param file The path of a .csv file formatted as described in 
#' \code{\link{read_plate}}.
#' @param well_ids_column The name of the column in \code{data} containing the 
#' well IDs. 
#' @return Returns data with as many new columns as plates in \code{file}. 
#' Empty wells are indicated with NA.
#' @export
#' @examples 
#' # Part of the data is tidy
#' file <- system.file("extdata", "example-2-part-A.csv", package = "plater")
#' data <- read.csv(file)
#' 
#' # Part of the data is plate-shaped
#' plate_shaped <- system.file("extdata", "example-2-part-B.csv", package = "plater")
#' 
#' # Combine the two
#' data <- add_plate(
#'    data = data, 
#'    file = plate_shaped,
#'    well_ids_column = "Wells")
#' 
#' # Now data are tidy
#' head(data)
add_plate <- function(data, file, well_ids_column) {
   if ("data.frame" %in% class(file) && class(data) == "character") {
      warning("file and class arguments to add_plate appear to be reversed.")
      temp <- data
      data <- file
      file <- temp
   }
   
   # save for later use
   original_class <- class(data)
   
   to_add <- read_plate(file, "wellIds")
   
   # validate well_ids_column
   check_well_ids_column_name(well_ids_column)
   validate_column_is_in_data(data, well_ids_column)
   
   # validate well IDs
   missing_leading_zeroes <- are_leading_zeroes_missing(data, well_ids_column, 
      guess_plate_size(file))   
   
   if(missing_leading_zeroes) {
      to_add$wellIds <- remove_leading_zeroes(to_add$wellIds)
   }
   
   # ensure data has all wells that the file does
   #if(!(all(to_add$wellIds %in% data[[well_ids_column]]))) {
    #  stop(wrong_wells_error_message(data, well_ids_column, to_add))
   #}
   
   # merge new columns with input data frame
   result <- merge(data, to_add, by.x = well_ids_column, by.y = "wellIds", 
      all.x = TRUE, all.y = TRUE) 
      # all.x adds NA rows for wells missing from file
      # all.y adds NA rows for wells missing from data
   
   # maintain order provided by user
   # wells in plate layout but not data frame from user will be appended
   user_order <- order(match(
         result[[well_ids_column]], 
         data[[well_ids_column]]))
   
   result <- result[user_order, ]
   
   # add to class (use union bc it might have other classes eg grouped_df)
   class(result) <- union(original_class, c("tbl_df", "tbl", "data.frame"))
   result
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
wrong_wells_error_message <- function(data, well_ids_column, annotations) {
   missing <- annotations$wellIds[!(annotations$wellIds %in% 
         data[[well_ids_column]])]
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
are_leading_zeroes_missing <- function(data, well_ids_column, plate_size) {
   if ((all(data[[well_ids_column]] %in% get_well_ids(plate_size)))) {
      return(FALSE)
   } else {
      if (!are_leading_zeroes_valid(data, well_ids_column, plate_size)) {
         # leading zeroes are invalid
         if(!(all(data[[well_ids_column]] %in% 
               get_well_ids_without_leading_zeroes(plate_size)))) {
            # some missing leading zeroes, some not, give up
            stop("Invalid well IDs--some have leading zeroes and some don't.", 
               call. = FALSE)
         } else {
            return(TRUE)
         }
      } else {
         # problem is not with leading zeroes
         stop("Some well IDs are invalid.", call. = FALSE)
      }
   }
}