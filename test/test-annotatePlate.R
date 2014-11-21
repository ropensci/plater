require(testthat)
setwd("H:\\R\\annotatePlate\\test")
source("../annotatePlate.R")
path <- paste0(getwd(), "/testData/")

################################################################################
context("testing validatePlate()")
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
context("testing wrongRowLabelsErrorMessage()")
################################################################################
