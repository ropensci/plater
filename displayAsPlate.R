id <- sapply(formatC(1:12, width = 2, flag = "0"),
   FUN = function(i) paste(LETTERS[1:8], i, sep = ""))
id <- as.vector(t(id))

data <- data.frame(wellIds = id, values = sample(96) + 0.0001)


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
      result
}

# how many decimals or whatever to show? 