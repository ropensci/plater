path <- "testData/"

################################################################################
context("testing annotatePlate-validatePlate()")
################################################################################

test_that("validate plate fails for incorrect 96-well plate dimensions", {
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

test_that("validatePlate() fails for incorrect 96-well row labels", {
   wrongRowLabels <- readPlate(paste0(path, "incorrectRowLabels.csv"))
   expect_that(validatePlate(wrongRowLabels), throws_error()) 
   
   wrongRowLabels <- readPlate(paste0(path, "missingRowLabels.csv"))
   expect_that(validatePlate(wrongRowLabels), throws_error()) 
})

test_that("validatePlate() passes with valid 96-well input", {
   # no error
   plate <- readPlate(paste0(path, "validPlate96Well.csv"))
   validatePlate(plate)
   
   # missing column data, but includes all titles
   plate <- readPlate(paste0(path, "missingColumnsWithCorrectTitles.csv"))
   validatePlate(plate)
})

test_that("validate plate fails for incorrect 384-well plate dimensions", {
   bottomRow <- readPlate(paste0(path, "384missingBottomRow.csv"))
   expect_that(validatePlate(bottomRow), throws_error())   
   
   rightColumn <- readPlate(paste0(path, "384missingRightColumn.csv"))
   expect_that(validatePlate(rightColumn), throws_error())
   
   missingMiddle <- readPlate(paste0(path, "384missingMiddleRow.csv"))
   expect_that(validatePlate(missingMiddle), throws_error())   
   
   extraRow <- readPlate(paste0(path, "384oneExtraRow.csv"))
   expect_that(validatePlate(extraRow), throws_error())   
   
   extraCol <- readPlate(paste0(path, "384oneExtraColumn.csv"))
   expect_that(validatePlate(extraCol), throws_error())
})

test_that("validatePlate() fails for incorrect 384-well row labels", {
   wrongRowLabels <- readPlate(paste0(path, "384incorrectRowLabels.csv"))
   expect_that(validatePlate(wrongRowLabels), throws_error()) 

   wrongRowLabels <- readPlate(paste0(path, "384missingRowLabels.csv"))
   expect_that(validatePlate(wrongRowLabels), throws_error()) 
})

test_that("validatePlate() passes with valid 384-well input", {
   # no error
   plate <- readPlate(paste0(path, "validPlate384Well.csv"))
   validatePlate(plate)
})

################################################################################
context("testing annotatePlate-wrongRowLabelsErrorMessage()")
################################################################################
test_that("wrongRowLabelsErrorMessage() fails for valid plate", {
   validPlate <- readPlate(paste0(path, "validPlate384Well.csv"))
   expect_that(wrongRowLabelsErrorMessage(validPlate), throws_error())
})

test_that("wrongRowLabelsErrorMessage() fails for invalid plate dimensions", {
   missingRow <- readPlate(paste0(path, "missingBottomRow.csv"))
   expect_that(wrongRowLabelsErrorMessage(validPlate), throws_error())
})

test_that("wrongRowLabelsErrorMessage() 96-well", {
   incorrectRowLabels <- readPlate(paste0(path, "incorrectRowLabels.csv"))
   message <- wrongRowLabelsErrorMessage(incorrectRowLabels)
   expect_that(message, matches(
      paste0("Correct row labels not found. Found 'X B C D E F G H' but ",
               "expected 'a b c d e f g h' or 'A B C D E F G H'.")))
})

test_that("wrongRowLabelsErrorMessage() 384-well", {
   incorrectRowLabels384 <- readPlate(paste0(path, "384incorrectRowLabels.csv"))
   message <- wrongRowLabelsErrorMessage(incorrectRowLabels384)
   expect_that(message, matches(
      paste0("Correct row labels not found. Found 'A B C D E F G H N P K L M N",
            " O P' but expected 'a b c d e f g h i j k l m n o p' or 'A B C D",
            " E F G H I J K L M N O P'.")))
})

################################################################################
context("testing annotatePlate-annotate96WellPlate")
################################################################################
test_that("annotate96WellPlate() gives correct output", {
   # every well present, all have own ID as contents
   plate <- annotate96WellPlate(paste0(path, "allWellIds.csv"), "contents")
   expect_that(plate$contents, is_identical_to(plate$wellIds))
})

test_that("annotate96WellPlate() gives correct output with some empty wells", {
   # every well present, all have own ID as contents
   plate <- annotate96WellPlate(paste0(path, "wellIdsAndEmptyWells.csv"), "contents")
   expect_that(sum(is.na(plate$contents)), equals(27))
   plate <- plate[!is.na(plate$contents), ]
   expect_that(plate$contents, is_identical_to(plate$wellIds))
})

test_that("annotate96WellPlate() gives correct output with blank columnName", {
   # every well present, all have own ID as contents
   plate <- annotate96WellPlate(paste0(path, "allWellIds.csv"))
   expect_that(plate$values, is_identical_to(plate$wellIds))
})