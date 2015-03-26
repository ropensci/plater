for (i in c(12, 24, 48, 96, 384)) {
   path <- paste0("testData/", i, "/")
   
   ################################################################################
   context("testing convertOnePlate-validatePlate()")
   ################################################################################
   
   test_that("validate plate fails for incorrect plate dimensions", {
      bottomRow <- readPlate(paste0(path, "missingBottomRow.csv"))
      expect_that(validatePlate(bottomRow), throws_error())   
      
      rightColumn <- readPlate(paste0(path, "missingRightColumn.csv"))
      expect_that(validatePlate(rightColumn), throws_error())
      
      missingMiddle <- readPlate(paste0(path, "missingMiddleRow.csv"))
      expect_that(validatePlate(missingMiddle), throws_error())   
     
      extraRow <- readPlate(paste0(path, "oneExtraRow.csv"))
      expect_that(validatePlate(extraRow), throws_error())   
      
      extraCol <- readPlate(paste0(path, "oneExtraColumn.csv"))
      expect_that(validatePlate(extraCol), throws_error())
   })
   
   test_that("validatePlate() fails for incorrect row labels", {
      wrongRowLabels <- readPlate(paste0(path, "incorrectRowLabels.csv"))
      expect_that(validatePlate(wrongRowLabels), throws_error()) 
      
      wrongRowLabels <- readPlate(paste0(path, "missingRowLabels.csv"))
      expect_that(validatePlate(wrongRowLabels), throws_error()) 
   })
   
   test_that("validatePlate() passes with valid input", {
      # no error
      plate <- readPlate(paste0(path, "validPlate.csv"))
      validatePlate(plate, i)
      
      # missing column data, but includes all titles
      plate <- readPlate(paste0(path, "missingColumnsWithCorrectTitles.csv"))
      validatePlate(plate, i)
   })
   
   ################################################################################
   context("testing convertOnePlate-wrongRowLabelsErrorMessage()")
   ################################################################################
   
   test_that("wrongRowLabelsErrorMessage() fails for invalid plate dimensions", {
      missingRow <- readPlate(paste0(path, "missingBottomRow.csv"))
      expect_that(wrongRowLabelsErrorMessage(validPlate, i), throws_error())
   })
   
   test_that("wrongRowLabelsErrorMessage()", {
      incorrectRowLabels <- readPlate(paste0(path, "incorrectRowLabels.csv"))
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
   test_that("convertOnePlate() gives correct output", {
      # every well present, all have own ID as contents
      plate <- convertOnePlate(paste0(path, "allWellIds.csv"), i, "contents")
      expect_that(plate$contents, is_identical_to(plate$wellIds))
   })
   
   test_that("convertOnePlate() gives correct output with some empty wells", {
      # every well present, all have own ID as contents
      plate <- convertOnePlate(paste0(path, "wellIdsAndEmptyWells.csv"), i, "contents")
      expectedNAs <- c("12" = 1, "24" = 1, "48" = 5, "96" = 27, "384" = 30)
      expect_that(sum(is.na(plate$contents)), 
         is_equivalent_to(expectedNAs[as.character(i)]))
      plate <- plate[!is.na(plate$contents), ]
      expect_that(plate$contents, is_identical_to(plate$wellIds))
   })
}
