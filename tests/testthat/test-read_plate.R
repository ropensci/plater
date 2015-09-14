for (i in c(12, 24, 48, 96, 384)) {
   path <- paste0("testData/", i, "/")
   ################################################################################
   context("testing read_plate-read_plate()")
   ################################################################################
   
   test_that("read_plate works for complete valid data", {
      filename <- paste0(path, "allWellIds.csv")
   
      result <- read_plate(filename, "wells", i)
      expect_that(result$values, is_identical_to(getWellIds(i)))
      expect_that(result$values, is_identical_to(result$wells))
   })
   
   test_that("read_plate works for missing data from plate", {
      filename <- paste0(path, "wellIdsAndEmptyWells.csv")
      
      result <- read_plate(filename, "wells", i)
      
      expect_that(result$wells, is_identical_to(result$values))
   })

   test_that("read_plate works with one full plate and one partially empty", {
      filename <- c(paste0(path, "oneFullOnePartEmpty.csv"))
      
      result <- read_plate(filename, "wells", i)
      expect_that(result$full, is_identical_to(getWellIds(i)))
      expect_that(result$full, is_identical_to(result$wells))
      r <- is.na(result$partial) | result$partial == as.character(result$wells)
      expect_that(all(r), is_true())
   })
   
   ################################################################################
   context("testing read_plate-calculateNumberOfPlates()")
   ################################################################################
   makeRawFile <- function(filename) {
      readLines(paste0(path, filename, ".csv"))
   }
   
   test_that("calculateNumberOfPlates works with one plate no blank row", {
      raw <- makeRawFile("allWellIds")   
      
      result <- calculateNumberOfPlates(raw, numberOfRows(i))
      expect_that(result, is_identical_to(1))
   })
   
   test_that("calculateNumberOfPlates works with one plate with blank row", {
      raw <- makeRawFile("onePlateOneBlankRow")   
      
      result <- calculateNumberOfPlates(raw, numberOfRows(i))
      expect_that(result, is_identical_to(1))
   })
   
   test_that("calculateNumberOfPlates fails with one plate with 2 blank rows", {
      raw <- makeRawFile("onePlateTwoBlankRows")   
      
      expect_that(calculateNumberOfPlates(raw, numberOfRows(i)), throws_error())
   })
   
   test_that("calculateNumberOfPlates works with two plates no blank row", {
      raw <- makeRawFile("twoPlatesNoBlankRow")   
      
      result <- calculateNumberOfPlates(raw, numberOfRows(i))
      expect_that(result, is_identical_to(2))
   })
   
   test_that("calculateNumberOfPlates works with two plates with blank row", {
      raw <- makeRawFile("twoPlatesOneBlankRow")   
      
      result <- calculateNumberOfPlates(raw, numberOfRows(i))
      expect_that(result, is_identical_to(2))
   })
   
   test_that("calculateNumberOfPlates fails with two plates with 2 blank rows", {
      raw <- makeRawFile("twoPlatesTwoBlankRows")   
      
      expect_that(calculateNumberOfPlates(raw, numberOfRows(i)), throws_error())
   })
}

################################################################################
context("testing read_plate-checkUniquePlateNames()")
################################################################################
   test_that("duplicate plate names get disambiguated", {
      plate <- read_plate(paste0(path, "twoPlatesNoBlankRow.csv"), "wells", i) 
      
      expect_true("values" %in% colnames(plate))
      expect_true("values.2" %in% colnames(plate))
   })
   
   test_that("non-duplicate plate names are unchanged", {
      plate <- read_plate(paste0(path, "oneFullOnePartEmpty.csv"), "wells", i) 
      
      expect_true("full" %in% colnames(plate))
      expect_true("partial" %in% colnames(plate))
   })
   
   test_that("both empty plate names get filled in and disambiguated", {
      plate <- read_plate(paste0(path, "missingPlateNames.csv"), "wells", i) 
      
      expect_true("values" %in% colnames(plate))
      expect_true("values.2" %in% colnames(plate))
   })