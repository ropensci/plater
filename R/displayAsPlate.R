# how many decimals or whatever to show? 


#' Displays the data in the form of a microtiter plate
#'
#' @param data
#' @param wellIdColumn
#' @param columnToDisplay
#' @return xxx
displayAs96WellPlate <- function(data, wellIdColumn, columnToDisplay) {
   # get plate size
   
   
   # ensure the well IDs are correct
   data <- ensureCorrectWellIds(data, wellIdColumn, 96)
   
   # transform
   # sort by wellIds 
   data <- data[order(data[ , wellIdColumn]), ]
   
   # get data to display and replace NA with '.'
   toDisplay <- data[[columnToDisplay]]
   toDisplay <- ifelse(is.na(toDisplay), ".", toDisplay)
   
   # create result and name rows and columns
   result <- data.frame(matrix(toDisplay, nrow = 8, byrow = TRUE))
   rownames(result) <- LETTERS[1:8]
   colnames(result) <- 1:12
   
   return(result)
}

#' Returns \code{data} with updated well IDs if needed.
#'
#' Well-formed well IDs are of the form A01..H12 (for 96-well plates). That is, 
#' they have leading zeroes, each is unique, and every expected well ID is 
#' present. \code{ensureCorrectWellIds} fills in missing well IDs (with NA in 
#' other columns) and leading zeroes. It throws an error if there are more rows
#' than wells for that plate size, if any well IDs are duplicated, or if any 
#' well IDs are invalid for that plate size.   
#'
#' @param data A data frame
#' @param wellIdColumn The name of the column in data containing the well IDs
#' @param plateSize The size of the plate
#' @return Data with valid well IDs
ensureCorrectWellIds <- function(data, wellIdColumn, plateSize) {
   
   # stop if plateSize is invalid plate size
   
   wells <- data[[wellIdColumn]]
   trueWells <- getWellIds(plateSize)
   if (length(wells) > plateSize) {
      stop(paste0("There are more rows in your data ", 
         "frame than wells in the plate size you specified. In other words, ",
         "data$", wellIdColumn, " has ", length(wells), " elements, which is ",
         "longer than plateSize = ", plateSize))
   }
   #if any well IDs are wrong, stop
   
   if (areWellIdsCorrect(wells, 96)) {
      return(data)
   } else {
      # else take corrective action
         # if leading zeroes are missing
            # correct them
            # recursively call this function
      if (length(wells) < plateSize) {
         return(fillInMissingWellIds(data, wellIdColumn, plateSize))
      }
            # fill in missing with blanks
         # else 
            # stop   
   }
}

#' Returns TRUE if wells contains exactly the well IDs expected for plateSize.  
#'
#' @param wells A vector containing the well IDs. 
#' @param plateSize The size of the plate.
#' @return TRUE if wells is the same length as plateSize and contains every well 
#' ID expected for that plate size.
areWellIdsCorrect <- function(wells, plateSize) {
   if (length(wells) != plateSize) {
      return(FALSE)
   }
   trueWells <- getWellIds(plateSize)
   wells <- wells[order(wells)]

   return(all(wells == trueWells))
}

#' Returns \code{data} with valid well IDs and NAs for all other columns in new 
#' rows. 
#'
#' @inheritParams ensureCorrectWellIds 
#' @return Data with valid well IDs
fillInMissingWellIds <- function(data, wellIdColumn, plateSize) {
   if (nrow(data) >= plateSize) {
      stop(paste0("data has ", nrow(data), " rows, which is >= the plate size ",
         "(", plateSize, "). It should have fewer rows."))
   }
   
   # find which are missing
   wells <- as.character(data[, wellIdColumn])
   complete <- getWellIds(plateSize)
   missing <- !(complete %in% wells)

   # create replacement data frame
   wellsToAdd <- complete[missing]
   temp <- data[0 , -which(colnames(data) == wellIdColumn)]
   temp[1:length(wellsToAdd), ] <- NA
   
   # cbind replacement and column with wells
   tempNames <- colnames(temp)
   temp <- cbind(temp, wellsToAdd)
   
   # rename column with wells to user's name
   colnames(temp) <- c(tempNames, wellIdColumn)
   
   # if user provided factor wellIds, make sure full set of levels are there 
   if (is.factor(data$wells)) {
      data[, wellIdColumn] <- factor(data[, wellIdColumn], levels = complete)
      temp[, wellIdColumn] <- factor(temp[, wellIdColumn], levels = complete)
   }
   
   return(rbind(data, temp))
}

#' Returns \code{data} with leading zeroes added to well IDs missing them.
#'
#' @inheritParams ensureCorrectWellIds 
#' @return Data with correct leading zeroes in well IDs
correctLeadingZeroes <- function(data, wellIdColumn, plateSize) {
   # convert to character and store if needed to be changed back to factor   
   wasFactor <- FALSE
   if(is.factor(data[[wellIdColumn]])) {
      wasFactor <- TRUE
      data[, wellIdColumn] <- as.character(data[, wellIdColumn]) 
   }   
   
   # build lookup table
   missing <- getWellIdsWithoutLeadingZeroes(plateSize)
   correct <- getWellIds(plateSize)
   lookup <- data.frame(correct = correct, missing = missing, 
      stringsAsFactors = FALSE)
   
   # look up and add results as new column to data
   matches <- match(data[ , wellIdColumn], lookup$missing)
   data$temp <- lookup$correct[matches]
   
   # replace well ID with itself or with the value from the lookup table
   data[ , wellIdColumn] <- ifelse(is.na(data$temp), 
         as.character(data[ , wellIdColumn]), 
         as.character(data$temp))

   # remove temporary column
   data <- data[ , !(names(data) =="temp")]
   
   # return to factor if needed
   if(wasFactor) {
      data[ , wellIdColumn] <- factor(data[ , wellIdColumn])
   }

   return(data)
}