#' Check whether a file is in plater format. 
#' 
#' Runs the provided file through a number of diagnostics to determine whether
#' it is a valid plater format file and displays information about any 
#' deficiencies found. 
#' 
#' @param file The path of the file to check.
#' @param sep The character used to separate columns in the file (e.g. "," or ";"). 
#' Defaults to ",".
#' @return Displays a number of messages as it checks the file. Will stop with
#' a descriptive error message if the file is not formatted correctly. 
#' 
#' @export
#' @examples 
#' file_path <- system.file("extdata", "example-1.csv", package = "plater")
#' 
#' data <- check_plater_format(file_path)
check_plater_format <- function(file, sep = ",") {
  check <- function(description, f) {
    message(paste0("* ", description, " ... ", collapse = ""), appendLF = FALSE)
    result <- tryCatch(
      expr = f(), 
      error = function(e) {
        # new line
        message("problem.")
        stop(e)
      })
    message("good!")
    result
  }
  
  check("Checking file path", function() check_file_path(file))
  
  check("Checking that file is not empty", 
    function() check_that_file_is_non_empty(file))
  
  plate_size <- check("Checking valid column labels", 
    function() guess_plate_size(file, sep))
  
  raw_file_list <- check("Checking file length and number of plate layouts",
    function() get_list_of_plate_layouts(file, plate_size))
  
  result <- check("Checking plate dimensions and row labels", 
    function() convert_all_layouts(raw_file_list, plate_size, sep))
  
  message("Success!")
}
