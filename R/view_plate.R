#' Displays the data in the form of a microtiter plate. 
#'
#' @inheritParams read_plate
#' @param data A data frame containing the data
#' @param column_to_display The column containing the data to display.
#' @param well_ids_column The name of the column in \code{data} containing the 
#' well IDs. 
#' @param plate_size The number of wells in the plate. Must be 12, 24, 48, 96 or
#'  384. Default 96.
#' @inheritParams read_plate
#' @return A depiction of the data in \code{column_to_display} as 
#' though laid out on a microtiter plate with \code{plate_size} wells.
#' @export
#' @examples 
#' # Generate some tidy data
#' data <- data.frame(Wells = paste0(LETTERS[1:3], 0, rep(1:4, each = 3)), 
#' Species = rep(c("Alien", "Human", "Cat"), 4), 
#' OxygenProduction = round(rnorm(12), 3))
#' head(data)
#' 
#' # See which had cells from which species
#' view_plate(data, "Wells", "Species", 12)
#' 
#' # See oxygen production from the different wells
#' view_plate(data, "Wells", "OxygenProduction", 12)
view_plate <- function(data, well_ids_column, column_to_display, 
                      plate_size = 96) {
   # validate column names
   check_well_ids_column_name(well_ids_column)
   validate_column_is_in_data(data, c(well_ids_column, column_to_display))

   n_rows <- number_of_rows(plate_size) # stops if not 12, 24, 48, 96 or 384
   n_columns <- number_of_columns(plate_size)
   
   # convert well IDs to character; if factor, order can be wrong
   data[ , well_ids_column] <- as.character(data[ , well_ids_column])
   
   # ensure the well IDs are correct
   data <- ensure_correct_well_ids(data, well_ids_column, plate_size)
   
   # transform
   # sort by wellIds 
   data <- data[order(data[ , well_ids_column]), ]
   
   # get data to display and replace NA with '.'
   to_display <- as.character(data[[column_to_display]])
   to_display <- ifelse(is.na(to_display), ".", to_display)
   
   # create result and name rows and columns
   result <- data.frame(matrix(to_display, nrow = n_rows, byrow = TRUE))
   rownames(result) <- LETTERS[1:n_rows]
   colnames(result) <- 1:n_columns
   
   return(result)
}

# Returns \code{data} with updated well IDs if needed.
#
# Well-formed well IDs are of the form A01..H12 (for 96-well plates). That is, 
# they have leading zeroes, each is unique, and every expected well ID is 
# present. \code{ensure_correct_well_ids} fills in missing well IDs (with NA in 
# other columns) and leading zeroes. It throws an error if there are more rows
# than wells for that plate size, if any well IDs are duplicated, or if any 
# well IDs are invalid for that plate size.   
#
# @param data A data frame
# @param well_ids_column The name of the column in data containing the well IDs
# @param plate_size The size of the plate
# @return Data with valid well IDs
ensure_correct_well_ids <- function(data, well_ids_column, plate_size) {
   wells <- data[[well_ids_column]]
   true_wells <- get_well_ids(plate_size) # stops if not 12, 24, 48, 96 or 384
   if (length(wells) > plate_size) {
      stop(paste0("There are more rows in your data ", 
         "frame than wells in the plate size you specified. In other words, ",
         "data$", well_ids_column, " has ", length(wells), " elements, which is ",
         "longer than plate_size = ", plate_size), call. = FALSE)
   }
   
   if (are_well_ids_correct(wells, plate_size)) {
      return(data)
   } else {
      if(!are_leading_zeroes_valid(data, well_ids_column, plate_size)) {
         data <- correct_leading_zeroes(data, well_ids_column, plate_size)
      }

      if (length(wells) < plate_size) {
         data <- fill_in_missing_well_ids(data, well_ids_column, plate_size)
      }
      
      if(are_well_ids_correct(data[[well_ids_column]], plate_size)) {
         return(data)
      } else {
         # some well IDs are duplicates or incorrect
         stop("Well IDs are invalid.", call. = FALSE)
      }
   }
}

# Returns TRUE if wells contains exactly the well IDs expected for plate_size.  
#
# @param wells A vector containing the well IDs. 
# @param plate_size The size of the plate.
# @return TRUE if wells is the same length as plate_size and contains every well 
# ID expected for that plate size.
are_well_ids_correct <- function(wells, plate_size) {
   if (length(wells) != plate_size) {
      return(FALSE)
   }
   true_wells <- get_well_ids(plate_size)
   wells <- wells[order(wells)]

   return(all(wells == true_wells))
}

# Returns \code{data} with the full set of valid well IDs for its size. 
# 
# Appends any well IDs missing from data$well_ids_column for the given plate size
# as new rows, with NAs in the other columns. 
# 
# All well IDs should have leading zeroes, if appropriate. 
#
# @inheritParams ensure_correct_well_ids 
# @return Data with valid well IDs
fill_in_missing_well_ids <- function(data, well_ids_column, plate_size) {
   if (nrow(data) >= plate_size) {
      stop(paste0("data has ", nrow(data), " rows, which is >= the plate size ",
         "(", plate_size, "). It should have fewer rows."), call. = FALSE)
   }
   
   if(!are_leading_zeroes_valid(data, well_ids_column, plate_size)) {
      stop("Some well IDs are missing leading zeroes.", call. = FALSE)
   }
   
   # find which are missing
   wells <- as.character(data[, well_ids_column])
   complete <- get_well_ids(plate_size)
   missing <- !(complete %in% wells)

   # create replacement data frame
   wells_to_add <- complete[missing]
   temp <- data[0 , -which(colnames(data) == well_ids_column), drop = FALSE]
   temp[1:length(wells_to_add), ] <- NA
   
   # cbind replacement and column with wells
   original_names <- colnames(temp)
   temp <- cbind(temp, wells_to_add)
   
   # rename column with wells to user's name
   colnames(temp) <- c(original_names, well_ids_column)
   
   # if user provided factor wellIds, make sure full set of levels are there 
   if (is.factor(data[[well_ids_column]])) {
      data[, well_ids_column] <- factor(data[, well_ids_column], levels = complete)
      temp[, well_ids_column] <- factor(temp[, well_ids_column], levels = complete)
   }
   
   return(rbind(data, temp))
}

# Returns TRUE if all well IDs that should have leading zeroes do.
#
# @inheritParams ensure_correct_well_ids 
# @return TRUE if all well IDs that should have leading zeroes do. This
# includes the case where no well IDs need leading zeroes (e.g. if all are >
# 9 or if none of the IDs are valid well IDs without leading zeroes). Thus this
# function returns TRUE for data$well_ids_column containing arbitrary, non-ID 
# text.
are_leading_zeroes_valid <- function(data, well_ids_column, plate_size) {
   wells <- data[[well_ids_column]]
   missing <- get_well_ids_without_leading_zeroes(plate_size)
   missing <- missing[nchar(missing) == 2]
   if (any(wells %in% missing)) {
      return(FALSE)
   }
   return(TRUE)
}

# Returns \code{data} with leading zeroes added to well IDs missing them.
#
# @inheritParams ensure_correct_well_ids 
# @return Data with correct leading zeroes in well IDs
correct_leading_zeroes <- function(data, well_ids_column, plate_size) {
   # convert to character and store if needed to be changed back to factor   
   was_factor <- FALSE
   if(is.factor(data[[well_ids_column]])) {
      was_factor <- TRUE
      data[, well_ids_column] <- as.character(data[, well_ids_column]) 
   }   
   
   # build lookup table
   missing <- get_well_ids_without_leading_zeroes(plate_size)
   correct <- get_well_ids(plate_size)
   lookup <- data.frame(correct = correct, missing = missing, 
      stringsAsFactors = FALSE)
   
   # look up and add results as new column to data
   matches <- match(data[ , well_ids_column], lookup$missing)
   data$temp <- lookup$correct[matches]
   
   # replace well ID with itself or with the value from the lookup table
   data[ , well_ids_column] <- ifelse(is.na(data$temp), 
         as.character(data[ , well_ids_column]), 
         as.character(data$temp))

   # remove temporary column
   data <- data[ , !(names(data) =="temp")]
   
   # return to factor if needed
   if(was_factor) {
      data[ , well_ids_column] <- factor(data[ , well_ids_column])
   }

   return(data)
}