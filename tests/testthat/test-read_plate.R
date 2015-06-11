for (i in c(12, 24, 48, 96, 384)) {
   path <- paste0("testData/", i, "/")
   ################################################################################
   context("testing read_plate-read_plate()")
   ################################################################################
   
   test_that("read_plate works for complete valid data", {
      filename <- paste0(path, "allWellIds.csv")
   
      result <- read_plate(i, "wells", filename, "values")
      expect_that(result$values, is_identical_to(getWellIds(i)))
      expect_that(result$values, is_identical_to(result$wells))
   })
   
   test_that("read_plate works for missing data from plate", {
      filename <- paste0(path, "wellIdsAndEmptyWells.csv")
      
      result <- read_plate(i, "wells", filename, "values")
      
      expect_that(result$wells, is_identical_to(result$values))
   })

   test_that("read_plate works with one full plate and one partially empty", {
      filename <- c("allWellIds.csv", "wellIdsAndEmptyWells.csv")
      filename <- c(paste0(path, filename))
      
      result <- read_plate(i, "wells", filename, c("full", "partial"))
      expect_that(result$full, is_identical_to(getWellIds(i)))
      expect_that(result$full, is_identical_to(result$wells))
      r <- is.na(result$partial) | result$partial == as.character(result$wells)
      expect_that(all(r), is_true())
   })

   test_that("read_plate error with unequal numbers of files/column names", {
      multipleFiles <- c("allWellIds.csv", "wellIdsAndEmptyWells.csv")
      multipleFiles <- c(paste0(path, multipleFiles))
      oneFile <- multipleFiles[1]
      
      multipleColNames <- c("full", "partial")
      oneColName <- "full"
      
      expect_that(read_plate(i, "wells", multipleFiles, oneColName), throws_error())
      expect_that(read_plate(i, "wells", oneFile, multipleColNames), throws_error())

   })
   
}