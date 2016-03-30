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
view_plate <- function(data, well_ids_column, column_to_display, 
                      plate_size = 96) {
   # validate well_ids_column
   validateColumnIsInData(data, well_ids_column)
   validateColumnIsInData(data, column_to_display)

   nRows <- numberOfRows(plate_size) # stops if not 12, 24, 48, 96 or 384
   nColumns <- numberOfColumns(plate_size)
   
   # convert well IDs to character; if factor, order can be wrong
   data[ , well_ids_column] <- as.character(data[ , well_ids_column])
   
   # ensure the well IDs are correct
   data <- ensureCorrectWellIds(data, well_ids_column, plate_size)
   
   # transform
   # sort by wellIds 
   data <- data[order(data[ , well_ids_column]), ]
   
   # get data to display and replace NA with '.'
   toDisplay <- as.character(data[[column_to_display]])
   toDisplay <- ifelse(is.na(toDisplay), ".", toDisplay)
   
   # create result and name rows and columns
   result <- data.frame(matrix(toDisplay, nrow = nRows, byrow = TRUE))
   rownames(result) <- LETTERS[1:nRows]
   colnames(result) <- 1:nColumns
   
   return(result)
}

# Returns \code{data} with updated well IDs if needed.
#
# Well-formed well IDs are of the form A01..H12 (for 96-well plates). That is, 
# they have leading zeroes, each is unique, and every expected well ID is 
# present. \code{ensureCorrectWellIds} fills in missing well IDs (with NA in 
# other columns) and leading zeroes. It throws an error if there are more rows
# than wells for that plate size, if any well IDs are duplicated, or if any 
# well IDs are invalid for that plate size.   
#
# @param data A data frame
# @param well_ids_column The name of the column in data containing the well IDs
# @param plate_size The size of the plate
# @return Data with valid well IDs
ensureCorrectWellIds <- function(data, well_ids_column, plate_size) {
   wells <- data[[well_ids_column]]
   trueWells <- getWellIds(plate_size) # stops if not 12, 24, 48, 96 or 384
   if (length(wells) > plate_size) {
      stop(paste0("There are more rows in your data ", 
         "frame than wells in the plate size you specified. In other words, ",
         "data$", well_ids_column, " has ", length(wells), " elements, which is ",
         "longer than plate_size = ", plate_size))
   }
   
   if (areWellIdsCorrect(wells, plate_size)) {
      return(data)
   } else {
      if(!areLeadingZeroesValid(data, well_ids_column, plate_size)) {
         data <- correctLeadingZeroes(data, well_ids_column, plate_size)
      }

      if (length(wells) < plate_size) {
         data <- fillInMissingWellIds(data, well_ids_column, plate_size)
      }
      
      if(areWellIdsCorrect(data[[well_ids_column]], plate_size)) {
         return(data)
      } else {
         # some well IDs are duplicates or incorrect
         stop("Well IDs are invalid.")
      }
   }
}

# Returns TRUE if wells contains exactly the well IDs expected for plate_size.  
#
# @param wells A vector containing the well IDs. 
# @param plate_size The size of the plate.
# @return TRUE if wells is the same length as plate_size and contains every well 
# ID expected for that plate size.
areWellIdsCorrect <- function(wells, plate_size) {
   if (length(wells) != plate_size) {
      return(FALSE)
   }
   trueWells <- getWellIds(plate_size)
   wells <- wells[order(wells)]

   return(all(wells == trueWells))
}

# Returns \code{data} with the full set of valid well IDs for its size. 
# 
# Appends any well IDs missing from data$well_ids_column for the given plate size
# as new rows, with NAs in the other columns. 
# 
# All well IDs should have leading zeroes, if appropriate. 
#
# @inheritParams ensureCorrectWellIds 
# @return Data with valid well IDs
fillInMissingWellIds <- function(data, well_ids_column, plate_size) {
   if (nrow(data) >= plate_size) {
      stop(paste0("data has ", nrow(data), " rows, which is >= the plate size ",
         "(", plate_size, "). It should have fewer rows."))
   }
   
   if(!areLeadingZeroesValid(data, well_ids_column, plate_size)) {
      stop("Some well IDs are missing leading zeroes.")
   }
   
   # find which are missing
   wells <- as.character(data[, well_ids_column])
   complete <- getWellIds(plate_size)
   missing <- !(complete %in% wells)

   # create replacement data frame
   wellsToAdd <- complete[missing]
   temp <- data[0 , -which(colnames(data) == well_ids_column), drop = FALSE]
   temp[1:length(wellsToAdd), ] <- NA
   
   # cbind replacement and column with wells
   originalNames <- colnames(temp)
   temp <- cbind(temp, wellsToAdd)
   
   # rename column with wells to user's name
   colnames(temp) <- c(originalNames, well_ids_column)
   
   # if user provided factor wellIds, make sure full set of levels are there 
   if (is.factor(data$wells)) {
      data[, well_ids_column] <- factor(data[, well_ids_column], levels = complete)
      temp[, well_ids_column] <- factor(temp[, well_ids_column], levels = complete)
   }
   
   return(rbind(data, temp))
}

# Returns TRUE if all well IDs that should have leading zeroes do.
#
# @inheritParams ensureCorrectWellIds 
# @return TRUE if all well IDs that should have leading zeroes do. This
# includes the case where no well IDs need leading zeroes (e.g. if all are >
# 9 or if none of the IDs are valid well IDs without leading zeroes). Thus this
# function returns TRUE for data$well_ids_column containing arbitrary, non-ID 
# text.
areLeadingZeroesValid <- function(data, well_ids_column, plate_size) {
   wells <- data[[well_ids_column]]
   missing <- getWellIdsWithoutLeadingZeroes(plate_size)
   missing <- missing[nchar(missing) == 2]
   if (any(wells %in% missing)) {
      return(FALSE)
   }
   return(TRUE)
}

# Returns \code{data} with leading zeroes added to well IDs missing them.
#
# @inheritParams ensureCorrectWellIds 
# @return Data with correct leading zeroes in well IDs
correctLeadingZeroes <- function(data, well_ids_column, plate_size) {
   # convert to character and store if needed to be changed back to factor   
   wasFactor <- FALSE
   if(is.factor(data[[well_ids_column]])) {
      wasFactor <- TRUE
      data[, well_ids_column] <- as.character(data[, well_ids_column]) 
   }   
   
   # build lookup table
   missing <- getWellIdsWithoutLeadingZeroes(plate_size)
   correct <- getWellIds(plate_size)
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
   if(wasFactor) {
      data[ , well_ids_column] <- factor(data[ , well_ids_column])
   }

   return(data)
}