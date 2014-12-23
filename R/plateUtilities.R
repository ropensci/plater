

#' requires:    plateSize == 96 or 384
#' returns:     a character vector of well IDs (e.g. A01..B05..H12) of length 96 
#'              or 384 
getWellIds <- function(plateSize) {
   cols <- numberOfColumns(plateSize)
   rows <- numberOfRows(plateSize)
   
   wells <- sapply(formatC(1:cols, width = 2, flag = "0"), 
      FUN = function(i) paste(LETTERS[1:rows], i, sep = ""))
   wells <- as.vector(t(wells))
   return(wells)
}

#' Returns a character vector of well IDs without leading zeroes (e.g. A1..B10..
#' H12) of length 96 or 384
#'
#' @param plateSize
#' @return
#' @examples
getWellIdsWithoutLeadingZeroes <- function(plateSize) {
   wells <- getWellIds(plateSize)
   wells <- ifelse(substr(wells, 2, 2) == "0", 
      paste0(substr(wells, 1, 1), substr(wells, 3, 3)), 
      wells)
   return(wells)
}

numberOfRows <- function(plateSize) {
   return(plateSize / numberOfColumns(plateSize))
}
         
numberOfColumns <- function(plateSize) {
   if(plateSize == 96) {
      cols = 12
   } else if (plateSize == 384) {
      cols = 24
   } else {
      stop(paste0("Invalid plateSize: ", plateSize, ". Must be 96 or 384."))
   }
}