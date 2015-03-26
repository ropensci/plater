#' Displays the data in the form of a microtiter plate. 
#'
#' @param data A data frame containing the data
#' @param wellIdsColumn The column containing the well IDs, which should be of
#' the form A01..B07..H12, though leading zeroes may be missing. Not all wells 
#' need to be included if some should be empty. Must not contain any well IDs
#' not on a plate with plateSize wells.
#' @param columnToDisplay The column containing the data to display.
#' @param plateSize The size of the plate. Must be 12, 24, 48, 96 or 384. 
#' @return A data frame representing the data in columnToDisplay as though laid 
#' out on a microtiter plate with plateSize wells.
#' @export
displayAsPlate <- function(data, wellIdsColumn, columnToDisplay, plateSize) {
   # validate wellIdsColumn
   validateWellIdsColumn(data, wellIdsColumn)
   
   nRows <- numberOfRows(plateSize) # stops if not 12, 24, 48, 96 or 384
   nColumns <- numberOfColumns(plateSize)
   
   # ensure the well IDs are correct
   data <- ensureCorrectWellIds(data, wellIdsColumn, plateSize)
   
   # transform
   # sort by wellIds 
   data <- data[order(data[ , wellIdsColumn]), ]
   
   # get data to display and replace NA with '.'
   toDisplay <- as.character(data[[columnToDisplay]])
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
# @param wellIdsColumn The name of the column in data containing the well IDs
# @param plateSize The size of the plate
# @return Data with valid well IDs
ensureCorrectWellIds <- function(data, wellIdsColumn, plateSize) {
   wells <- data[[wellIdsColumn]]
   trueWells <- getWellIds(plateSize) # stops if not 12, 24, 48, 96 or 384
   if (length(wells) > plateSize) {
      stop(paste0("There are more rows in your data ", 
         "frame than wells in the plate size you specified. In other words, ",
         "data$", wellIdsColumn, " has ", length(wells), " elements, which is ",
         "longer than plateSize = ", plateSize))
   }
   
   if (areWellIdsCorrect(wells, plateSize)) {
      return(data)
   } else {
      if(!areLeadingZeroesValid(data, wellIdsColumn, plateSize)) {
         data <- correctLeadingZeroes(data, wellIdsColumn, plateSize)
      }

      if (length(wells) < plateSize) {
         data <- fillInMissingWellIds(data, wellIdsColumn, plateSize)
      }
      
      if(areWellIdsCorrect(data[[wellIdsColumn]], plateSize)) {
         return(data)
      } else {
         # some well IDs are duplicates or incorrect
         stop("Well IDs are invalid.")
      }
   }
}

# Returns TRUE if wells contains exactly the well IDs expected for plateSize.  
#
# @param wells A vector containing the well IDs. 
# @param plateSize The size of the plate.
# @return TRUE if wells is the same length as plateSize and contains every well 
# ID expected for that plate size.
areWellIdsCorrect <- function(wells, plateSize) {
   if (length(wells) != plateSize) {
      return(FALSE)
   }
   trueWells <- getWellIds(plateSize)
   wells <- wells[order(wells)]

   return(all(wells == trueWells))
}

# Returns \code{data} with the full set of valid well IDs for its size. 
# 
# Appends any well IDs missing from data$wellIdsColumn for the given plate size
# as new rows, with NAs in the other columns. 
# 
# All well IDs should have leading zeroes, if appropriate. 
#
# @inheritParams ensureCorrectWellIds 
# @return Data with valid well IDs
fillInMissingWellIds <- function(data, wellIdsColumn, plateSize) {
   if (nrow(data) >= plateSize) {
      stop(paste0("data has ", nrow(data), " rows, which is >= the plate size ",
         "(", plateSize, "). It should have fewer rows."))
   }
   
   if(!areLeadingZeroesValid(data, wellIdsColumn, plateSize)) {
      stop("Some well IDs are missing leading zeroes.")
   }
   
   # find which are missing
   wells <- as.character(data[, wellIdsColumn])
   complete <- getWellIds(plateSize)
   missing <- !(complete %in% wells)

   # create replacement data frame
   wellsToAdd <- complete[missing]
   temp <- data[0 , -which(colnames(data) == wellIdsColumn), drop = FALSE]
   temp[1:length(wellsToAdd), ] <- NA
   
   # cbind replacement and column with wells
   originalNames <- colnames(temp)
   temp <- cbind(temp, wellsToAdd)
   
   # rename column with wells to user's name
   colnames(temp) <- c(originalNames, wellIdsColumn)
   
   # if user provided factor wellIds, make sure full set of levels are there 
   if (is.factor(data$wells)) {
      data[, wellIdsColumn] <- factor(data[, wellIdsColumn], levels = complete)
      temp[, wellIdsColumn] <- factor(temp[, wellIdsColumn], levels = complete)
   }
   
   return(rbind(data, temp))
}

# Returns TRUE if all well IDs that should have leading zeroes do.
#
# @inheritParams ensureCorrectWellIds 
# @return TRUE if all well IDs that should have leading zeroes do. This
# includes the case where no well IDs need leading zeroes (e.g. if all are >
# 9 or if none of the IDs are valid well IDs without leading zeroes). Thus this
# function returns TRUE for data$wellIdsColumn containing arbitrary, non-ID 
# text.
areLeadingZeroesValid <- function(data, wellIdsColumn, plateSize) {
   wells <- data[[wellIdsColumn]]
   missing <- getWellIdsWithoutLeadingZeroes(plateSize)
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
correctLeadingZeroes <- function(data, wellIdsColumn, plateSize) {
   # convert to character and store if needed to be changed back to factor   
   wasFactor <- FALSE
   if(is.factor(data[[wellIdsColumn]])) {
      wasFactor <- TRUE
      data[, wellIdsColumn] <- as.character(data[, wellIdsColumn]) 
   }   
   
   # build lookup table
   missing <- getWellIdsWithoutLeadingZeroes(plateSize)
   correct <- getWellIds(plateSize)
   lookup <- data.frame(correct = correct, missing = missing, 
      stringsAsFactors = FALSE)
   
   # look up and add results as new column to data
   matches <- match(data[ , wellIdsColumn], lookup$missing)
   data$temp <- lookup$correct[matches]
   
   # replace well ID with itself or with the value from the lookup table
   data[ , wellIdsColumn] <- ifelse(is.na(data$temp), 
         as.character(data[ , wellIdsColumn]), 
         as.character(data$temp))

   # remove temporary column
   data <- data[ , !(names(data) =="temp")]
   
   # return to factor if needed
   if(wasFactor) {
      data[ , wellIdsColumn] <- factor(data[ , wellIdsColumn])
   }

   return(data)
}