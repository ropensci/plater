# Reads and converts one plate to a data frame. 
#  
# convert_plate_to_column maps data from a microtiter plate layout to columns 
# identified by well names. Columns are named by the value held in the top-left
# cell of \code{plate} or called "value" if empty.
#
# @param plate A character vector with each element containing a comma-
# delimited row of a plate
# @param plate_size The number of wells in the plate
# @return Returns a two-column data frame, with one column called wellIds (A01, 
#              A02..) and the other containing the values in the indicated 
#              wells). Empty wells are omitted.  
convert_plate_to_column <- function(plate, plate_size) {
   plate <- plate_text_to_data_frame(plate)
   
   column_name <- plate[1, 1]
   
   if(is.na(column_name)) {
      column_name <- "values"
   }
   
   # remove column names
   plate <- plate[-1, ]
   
   # stop if plate is invalid
   validate_plate(plate, plate_size)
   
   # remove column of row labels
   plate <- plate[-1]
   
   # get plate dimensions
   rows <- nrow(plate)
   cols <- ncol(plate)
   
   # convert to vector
   plate <- unlist(lapply(seq_len(nrow(plate)), function(i) unname(plate[i, ])))
   # as.vector(t(plate)) is simpler, but t() calls as.matrix() and when you have 
   # a plate layout with one numeric column and one character column, all are
   # converted to character, but numeric goes to character via format() which 
   # adds white space to numeric columns to right-align numbers
   # see: as.matrix(data.frame(Numeric = c(1, 20), Character = "a"))   
   
   # generate well labels,
   wells <- get_well_ids(rows * cols)
   
   df <- data.frame(wellIds = wells, ColumnName = plate, 
         stringsAsFactors = FALSE)
   names(df) <- c("wellIds", column_name)
   
   # remove any NA values from the new column
   column <- colnames(df)[colnames(df) != "wellIds"]
   df <- df[!(is.na(df[, column])), ]
   
   return (df)   
}

# requires:    plate contains a character vector, as specified above
# returns:     a data frame created from the plate
plate_text_to_data_frame <- function(plate) {
   utils::read.table(textConnection(plate), sep = ",", 
      na.strings = "", stringsAsFactors = FALSE, comment.char = "")
}

# requires:    plate is non-null
# param:       plate    a data frame
# param:       plate_size expected plate size   
# throws:      stops if dimensions of plate (minus one column) are not (8, 12) 
#              or (16, 24) or if row labels are incorrect (not A:H or A:P)
validate_plate <- function(plate, plate_size) {
   if (!are_plate_dimensions_valid(plate, plate_size)) {
      stop(paste0("Invalid plate dimensions. Found ", nrow(plate), " rows and ", 
         ncol(plate) - 1, " columns. Must be (", number_of_rows(plate_size), ", ",
         number_of_columns(plate_size), ") for a ", plate_size, "-well plate."), 
         call. = FALSE)
   }
      
   if (!are_row_labels_valid(plate, plate_size)) {
      stop(wrong_row_labels_error_message(plate, plate_size))
   }
}

# requires:    plate is non-null and has at least one row and column
# param:       plate    a data frame
# param:       plate_size expected plate size   
# returns:     true if dimensions of plate (minus one column) are not (8, 12) 
#              or (16, 24)
are_plate_dimensions_valid <- function(plate, plate_size) {
   rows <- nrow(plate)
   cols <- ncol(plate) - 1
   
   expected_rows <- number_of_rows(plate_size)
   expected_cols <- number_of_columns(plate_size)
   
   return(rows == expected_rows && cols == expected_cols)
}

# requires:    plate is non-null and has at least 1 column
# param:       plate    a data frame
# param:       plate_size expected plate size   
# returns:     true if column 1 is letters[1:8] or [1:16]. It may be in upper-, 
#              lower-, or mixed-case.
are_row_labels_valid <- function(plate, plate_size) {
   rows <- number_of_rows(plate_size)
   
   rowLabels <- trim_white_space(tolower(plate[[1]]))
   
   return(identical(letters[1:rows], rowLabels)) 
}

# requires:    plate is non-null and has valid dimensions, but the row labels 
#              are incorrect
# param:       plate    a data frame
# param:       plate_size expected plate size
# returns:     an error message, describing the row labels found and the row 
#              labels that were expected
wrong_row_labels_error_message <- function(plate, plate_size) {
   if (!are_plate_dimensions_valid(plate, plate_size)) {
      stop("plate must have valid dimensions", call. = FALSE)
   }
   if (are_row_labels_valid(plate, plate_size)) {
      stop("row labels must be invalid", call. = FALSE)
   }
   found <- paste(plate[[1]], collapse = " ")
   
   rows <- number_of_rows(plate_size)
   
   lower <- paste(letters[1:rows], collapse = " ")
   upper <- paste(LETTERS[1:rows], collapse = " ")
   
   output <- paste("Correct row labels not found. Found '", found, 
      "' but expected '",lower, "' or '", upper, "'.", sep = "")
   
   return(output)
}

trim_white_space <- function(text) {
   white_space <- "^\\s+|\\s+$"
   
   gsub(white_space, replacement = "", text)
}