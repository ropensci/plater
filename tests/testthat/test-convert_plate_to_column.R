for (i in c(12, 24, 48, 96, 384)) {
   path <- paste0("testData/", i, "/")
   
   ################################################################################
   context("testing convert_plate_to_column-validatePlate()")
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
   
   test_that("validatePlate() passes with white space around row labels", {
      # no error
      # contains spaces in the row labels
      plate <- readPlate(getFileForValidatePlate("validPlateWithWhiteSpaceInRowNames.csv"))
      validatePlate(plate, i)
   })
   
   ################################################################################
   context("testing convert_plate_to_column-wrongRowLabelsErrorMessage()")
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
   context("testing convert_plate_to_column-convert_plate_to_column")
   ################################################################################
   getFileForConvertPlate <- function(file) {
      readLines(paste0(path, file))
   }
   
   test_that("convert_plate_to_column() gives correct output", {
      # every well present, all have own ID as contents
      plate <- convert_plate_to_column(getFileForConvertPlate("allWellIds.csv"), i)   
      expect_that(plate$values, is_identical_to(plate$wellIds))
   })
   
}
