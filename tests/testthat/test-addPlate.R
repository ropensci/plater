for (i in c(12, 24, 48, 96, 384)) {
   path <- paste0("testData/", i, "/")
   ################################################################################
   context("testing addPlate-addPlate()")
   ################################################################################
   nLetters <- ifelse(i == 12, 12, 24)
   
   test_that("addPlate works for complete valid data", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = getWellIds(i), d = letters[1:nLetters])
      complete$d <- as.character(complete$d)
   
      result <- addPlate(complete, i, "wells", filename, "values")
      expect_that(result$values, is_identical_to(getWellIds(i)))
      expect_that(factor(result$values), is_identical_to(result$wells))
   })
   
   test_that("addPlate works without leading zeroes", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = getWellIdsWithoutLeadingZeroes(i), 
         d = letters[1:nLetters])
      complete$d <- as.character(complete$d)
      
      result <- addPlate(complete, i, "wells", filename, "values")
      expect_that(result$wells, 
         is_identical_to(factor(getWellIdsWithoutLeadingZeroes(i))))
      expect_that(factor(removeLeadingZeroes(result$values)), 
         is_identical_to(result$wells))
   })
   
   test_that("addPlate works for missing data from plate", {
      filename <- paste0(path, "wellIdsAndEmptyWells.csv")
      complete <- data.frame(wells = getWellIds(i), d = letters[1:nLetters])
      complete$d <- as.character(complete$d)
      
      result <- addPlate(complete, i, "wells", filename, "values")
      
      expect_that(as.character(result$wells), is_identical_to(getWellIds(i)))
      r <- is.na(result$values) | result$values == as.character(result$wells)
      expect_that(all(r), is_true())
   })
   
   test_that(paste("addPlate works for missing data from",
         "plate and no leading zeroes"), {
      filename <- paste0(path, "wellIdsAndEmptyWells.csv")
      complete <- data.frame(wells = getWellIdsWithoutLeadingZeroes(i), 
         d = letters[1:nLetters])
      complete$d <- as.character(complete$d)
      
      result <- addPlate(complete, i, "wells", filename, "values")
      
      expect_that(result$wells, 
         is_identical_to(factor(getWellIdsWithoutLeadingZeroes(i))))
      result$values <- removeLeadingZeroes(result$values)
      r <- is.na(result$values) | result$values == as.character(result$wells)
      expect_that(all(r), is_true())
   })
   
   test_that("addPlate stops if wells missing from df", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = getWellIds(i), d = letters[1:nLetters])
      complete$d <- as.character(complete$d)
      
      expect_that(addPlate(complete[1:(i-1), ], i, "wells", 
         filename, "values"),
         throws_error())
   })
   
   test_that(paste("addPlate stops if some wells are missing",
      "leading zeroes and some aren't"), {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = getWellIds(i), d = letters[1:nLetters])
      complete$d <- as.character(complete$d)
      set.seed(10)
      complete$wells <- ifelse(rnorm(1) < 0, 
         removeLeadingZeroes(complete$wells), 
         complete$wells)
      
      expect_that(addPlate(complete, "wells", i,
         filename, "values"),
         throws_error())
   })
   
   ################################################################################
   context("testing addPlate-wrongWellsErrorMessage()")
   ################################################################################
   test_that("wrongWellsErrorMessage returns correct message with 1 well missing", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = getWellIds(i), d = letters[1:nLetters])
      complete$d <- as.character(complete$d)
      
      annotations <- convertOnePlate(filename, i, "values")
      annotations <- annotations[!(is.na(annotations$values)), ]
      
      message <- wrongWellsErrorMessage(complete[1:(i-1), ], "wells", annotations)
      missing <- getWellIds(i)[i] # get last well
      expected <- paste0("Some wells in your file are not in the data frame ",
         "you provided, but they all should be. The missing wells are: ", 
         missing, ".")
      expect_that(message, is_identical_to(expected))
   })
   
   test_that("wrongWellsErrorMessage returns correct message with 2 wells missing", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = getWellIds(i), d = letters[1:nLetters])
      complete$d <- as.character(complete$d)
      
      annotations <- convertOnePlate(filename, i, "values")
      annotations <- annotations[!(is.na(annotations$values)), ]
      
      message <- wrongWellsErrorMessage(complete[1:(i-2), ], "wells", annotations)
      missing <- getWellIds(i)[(i-1):i] # get last two wells
      missing <- paste0(missing, collapse = ", ")
      expected <- paste0("Some wells in your file are not in the data frame ",
         "you provided, but they all should be. The missing wells are: ", 
         missing, ".")
      expect_that(message, is_identical_to(expected))
   })
   
   test_that("wrongWellsErrorMessage returns correct message with all but 1 well missing", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = getWellIds(i), d = letters[1:nLetters])
      complete$d <- as.character(complete$d)
      
      annotations <- convertOnePlate(filename, i, "values")
      annotations <- annotations[!(is.na(annotations$values)), ]
      
      message <- wrongWellsErrorMessage(complete[1, ], "wells", annotations)
      missing <- getWellIds(i)[-1] # get all but first
      missing <- paste0(missing, collapse = ", ")
      expected <- paste0("Some wells in your file are not in the data frame ",
         "you provided, but they all should be. The missing wells are: ", 
         missing, ".")
      expect_that(message, is_identical_to(expected))
   })
   
   test_that("wrongWellsErrorMessage errors with valid input", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = getWellIds(i), d = letters[1:nLetters])
      complete$d <- as.character(complete$d)
      
      annotations <- convertOnePlate(filename, i, "values")
      annotations <- annotations[!(is.na(annotations$values)), ]
      
      expect_that(wrongWellsErrorMessage(complete, "wells", annotations), 
         throws_error())
   })
}