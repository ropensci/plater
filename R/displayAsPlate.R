id <- getWellIds(96) # from annotatePlate
data <- data.frame(wellIds = id, values = sample(96) + 0.01)
# how many decimals or whatever to show? 


#' Displays the data in the form of a microtiter plate
#'  
displayAs96WellPlate <- function(data) {
   # get plate size
   # ensure plate has all well IDs for its size
      # fill missing ones with blanks
   
   # transform
   # sort by wellIds 
   data <- data[order(data$wellIds), ]
   
   # create result and name rows and columns
   result <- data.frame(matrix(data$values, nrow = 8, byrow = TRUE))
   rownames(result) <- LETTERS[1:8]
   colnames(result) <- 1:12
   
   return(result)
}

# data & wellIdColumn provided by user
# plateSize provided by function
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
   
   if (areWellIdsCorrect(wells, 96)) {
      return(TRUE)
   } else {
      # else take corrective action
         # if length(wells) < plateSize
            # fill in missing with blanks
            # recursively call this function
         # if leading zeroes are missing
            # correct them
         # else 
            # stop   
   }
}

#' Returns true if wells is the same length as plateSize and contains every well 
#' ID for a plate of that size 
areWellIdsCorrect <- function(wells, plateSize) {
   if (length(wells) != plateSize) {
      return(FALSE)
   }
   trueWells <- getWellIds(plateSize)
   wells <- wells[order(wells)]

   return(all(wells == trueWells))
}

#' Returns data with NAs included for all missing wells 
fillInMissingWellIds <- function(data, wellIdColumn, plateSize) {
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
      temp$wellsToAdd <- factor(temp$wellsToAdd, levels = complete)
   }
   
   return(rbind(data, temp))
}