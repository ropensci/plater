#' Read a plater-formatted file and turn it into a tidy data frame.
#' 
#' Converts data from \code{plater} format to a data frame with one well 
#' per row identified by well name.
#'
#' @param file The path of a .csv file formatted as described below.
#' @param well_ids_column The name to give the column that will contain the well
#' identifiers. Default "Wells".
#' @return Returns a data frame with each well as a row. One column will be 
#' named with \code{well_ids_column} and contain the well names (A01, A02..). 
#' There will be as many additional columns as layouts in \code{file}. Empty 
#' wells are omitted.
#' 
#' @section \code{plater} format:
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
#' punctuation. Any cell may also be blank.
#' 
#' Note that Microsoft Excel will sometimes include cells that appear to be 
#' blank in the .csv files it produces, so the files may have spurious columns
#' or rows outside of the plate, causing errors. To solve this problem, copy and
#' paste just the cells within the plate to a fresh worksheet and save it.
#' 
#' @section Multiple columns: 
#' Multiple columns of information about a plate can be included in a single 
#' file. After the first plate, leave one row blank, and then add another plate
#' formatted as described above. (The "blank" row should appear as blank in a 
#' spreadsheet editor, but as a row of commas when viewed as plain text.) As 
#' many plates as necessary can be included in a single file (e.g. data 
#' measured, subject, treatment, replicate, etc.).
#'
#' @export
#' @examples 
#' file_path <- system.file("extdata", "example-1.csv", package = "plater")
#' 
#' # Data are stored in plate-shaped form
#' data <- read_plate(
#'    file = file_path,
#'    well_ids_column = "Wells")
#' 
#' # Now data are tidy
#' head(data)
read_plate <- function(file, well_ids_column = "Wells") {
   check_that_only_one_file_is_provided(file) 
   check_file_path(file)
   check_that_file_is_non_empty(file)
   check_well_ids_column_name(well_ids_column)
   
   plate_size <- guess_plate_size(file)
   
   raw_file_list <- get_list_of_plate_layouts(file, plate_size)
   
   result <- convert_all_layouts(raw_file_list, plate_size)
   
   result <- combine_list_to_dataframe(result)
   
   # rename well IDs column to whatever user chose
   colnames(result)[colnames(result) == "wellIds"] <- well_ids_column
   
   class(result) <- c("tbl_df", "tbl", "data.frame")
   
   result
}

# Wrapper around convert_plate_to_column that reports which layout generates an
# error to help users find it. 
# 
# Catches any errors thrown and prefaces the error message with 
# "Error in layout #x" where x is the number of the layout generating the error.
#
# @param raw_file_list The list of containing plates from readLines
# @param plate_size The number of wells in the plate. Must be 12, 24, 48, 96 or
#                   384. Default 96.
# @return A list of two-column data frames of the same length as the input list.
convert_all_layouts <- function(raw_file_list, plate_size) {

   convert <- function(f, layout_number) {
      tryCatch(
         expr = convert_plate_to_column(f, plate_size), 
         error = function(e) { 
            e <- paste0("Error in layout #", layout_number, ": ", 
                                 e$message)
            stop(e, call. = FALSE)
         }
      )
   }
   
   Map(f = convert, raw_file_list, 1:length(raw_file_list))
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
calculate_number_of_plates <- function(raw_file, number_of_rows) {
   is_integer <- function(x) x %% 1 == 0 
   
   result <- (length(raw_file) + 1) / (number_of_rows + 2)
   if (is_integer(result)) {
      return(result)
   } else {
      # file might end in blank line in which case we shouldn't add one
      result <- (length(raw_file)) / (number_of_rows + 2)
      if (raw_file[length(raw_file)] == "" || is_integer(result)) {
         return(result)
      } else {
         stop(paste0("File length is incorrect. Must be a multiple of the ", 
                     "number of rows in the plate plus a header row for each ",
                     "plate and a blank row between plates."), 
            call. = FALSE)
      }
   }
}

# Read in the data and convert to a list of plate layouts. 
# 
# @param file Path to the plate file
# @param plate_size The number of wells in the plate. Must be 12, 24, 48, 96 or
#                   384. Default 96.
get_list_of_plate_layouts <- function(file, plate_size) {
   # read in file
   raw_file <- readLines(file)
   
   # get list of data frames with new columns
   number_of_rows <- number_of_rows(plate_size)
   
   number_of_plates <- calculate_number_of_plates(raw_file, number_of_rows)
   
   raw_file_list <- lapply(1:number_of_plates, FUN =
         function(plate) {
            first_row <- (plate - 1) * (number_of_rows + 1) + plate
            last_row <- first_row + number_of_rows
            raw_file[first_row:last_row]
         }
   )
   
   raw_file_list
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
check_unique_plate_names <- function(result) {
   # get plate names
   plate_names <- vapply(result, FUN = function(x) colnames(x)[2], 
      FUN.VALUE = "character")
   
   if(any(duplicated(plate_names))) {
      duplicates <- which(duplicated(plate_names))
      
      # replace duplicate column names with .n 
      result <- lapply(1:length(result), FUN = function(n) {
         if (n %in% duplicates) {
            new_name <- paste0(colnames(result[[n]])[2], ".", n)
            colnames(result[[n]])[2] <- new_name
            result[[n]]
         } else {
            result[[n]]
         }
      })
   }
   return(result)
}

# Merge together list of dataframes into a single dataframe.
#
# @throws error is any column name is a duplicate, other than "wellIds"
#
# @param result List, with each element containing a dataframe, with a column
#        named "wellIds"
# @return A dataframe merged together from the list elements. Wells are included
#         if they're non-missing in at least one of the data frames, otherwise
#         omitted.
combine_list_to_dataframe <- function(result) {
   if (length(result) == 1) {
      result <- result[[1]]
   } else {
      # ensure that plate names are unique
      result <- check_unique_plate_names(result)
      
      # combine result into one data frame
      result <- Reduce(function(x, y) merge(x, y, by = "wellIds", all = TRUE), 
         result)      
   }
   
   # only return rows which have value for more than the well ID
   keep <- rowSums(!is.na(result)) > 1
   
   result[keep, ]
}