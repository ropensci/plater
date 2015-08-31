for (i in c(12, 24, 48, 96, 384)) {
   path <- paste0("testData/", i, "/")
   
   ################################################################################
   context("testing convertOnePlate-validatePlate()")
   ################################################################################
   getFileForValidatePlate <- function(file) {
      result <- readLines(paste0(path, file))
      
      # remove top row
      result[2:(length(result))]
   }
   
   test_that("validate plate fails for incorrect plate dimensions", {
      bottomRow <- readPlate(getFileForValidatePlate("missingBottomRow.csv"))
      expect_that(validatePlate(bottomRow), throws_error())   
      
      rightColumn <- readPlate(getFileForValidatePlate("missingRightColumn.csv"))
      expect_that(validatePlate(rightColumn), throws_error())
      
      missingMiddle <- readPlate(getFileForValidatePlate("missingMiddleRow.csv"))
      expect_that(validatePlate(missingMiddle), throws_error())   
     
      extraRow <- readPlate(getFileForValidatePlate("oneExtraRow.csv"))
      expect_that(validatePlate(extraRow), throws_error())   
      
      extraCol <- readPlate(getFileForValidatePlate("oneExtraColumn.csv"))
      expect_that(validatePlate(extraCol), throws_error())
   })
   
   test_that("validatePlate() fails for incorrect row labels", {
      wrongRowLabels <- readPlate(getFileForValidatePlate("incorrectRowLabels.csv"))
      expect_that(validatePlate(wrongRowLabels), throws_error()) 
      
      wrongRowLabels <- readPlate(getFileForValidatePlate("missingRowLabels.csv"))
      expect_that(validatePlate(wrongRowLabels), throws_error()) 
   })
   
   test_that("validatePlate() passes with valid input", {
      # no error
      plate <- readPlate(getFileForValidatePlate("validPlate.csv"))
      validatePlate(plate, i)
      
      # missing column data, but includes all titles
      plate <- readPlate(getFileForValidatePlate("missingColumnsWithCorrectTitles.csv"))
      validatePlate(plate, i)
   })
   
   ################################################################################
   context("testing convertOnePlate-wrongRowLabelsErrorMessage()")
   ################################################################################
   
   test_that("wrongRowLabelsErrorMessage() fails for invalid plate dimensions", {
      missingRow <- readPlate(getFileForValidatePlate("missingBottomRow.csv"))
      expect_that(wrongRowLabelsErrorMessage(validPlate, i), throws_error())
   })
   
   test_that("wrongRowLabelsErrorMessage()", {
      incorrectRowLabels <- readPlate(getFileForValidatePlate("incorrectRowLabels.csv"))
      message <- wrongRowLabelsErrorMessage(incorrectRowLabels, i)
      
      rows <- numberOfRows(i)
      wrong <- paste(c("X", LETTERS[2:rows]), collapse = " ")
      lower <- paste(letters[1:rows], collapse = " ")
      upper <- paste(LETTERS[1:rows], collapse = " ")
      
      expect_that(message, matches(
         paste0("Correct row labels not found. Found '", wrong, "' but ",
                  "expected '", lower, "' or '", upper,"'.")))
   })
   
   ################################################################################
   context("testing convertOnePlate-convertOnePlate")
   ################################################################################
   getFileForConvertPlate <- function(file) {
      readLines(paste0(path, file))
   }
   
   test_that("convertOnePlate() gives correct output", {
      # every well present, all have own ID as contents
      plate <- convertOnePlate(getFileForConvertPlate("allWellIds.csv"), i)   
      expect_that(plate$values, is_identical_to(plate$wellIds))
   })
   
   test_that("convertOnePlate() gives correct output with some empty wells", {
      # every well present, all have own ID as contents
      plate <- convertOnePlate(getFileForConvertPlate("wellIdsAndEmptyWells.csv"), i)
      expectedNAs <- c("12" = 1, "24" = 1, "48" = 5, "96" = 27, "384" = 30)
      expect_that(sum(is.na(plate$values)), 
         is_equivalent_to(expectedNAs[as.character(i)]))
      plate <- plate[!is.na(plate$values), ]
      expect_that(plate$values, is_identical_to(plate$wellIds))
   })
}
