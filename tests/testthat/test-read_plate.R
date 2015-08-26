for (i in c(12, 24, 48, 96, 384)) {
   path <- paste0("testData/", i, "/")
   ################################################################################
   context("testing read_plate-read_plate()")
   ################################################################################
   
   test_that("read_plate works for complete valid data", {
      filename <- paste0(path, "allWellIds.csv")
   
      result <- read_plate(i, "wells", filename)
      expect_that(result$values, is_identical_to(getWellIds(i)))
      expect_that(result$values, is_identical_to(result$wells))
   })
   
   test_that("read_plate works for missing data from plate", {
      filename <- paste0(path, "wellIdsAndEmptyWells.csv")
      
      result <- read_plate(i, "wells", filename)
      
      expect_that(result$wells, is_identical_to(result$values))
   })

   test_that("read_plate works with one full plate and one partially empty", {
      filename <- c(paste0(path, "oneFullOnePartEmpty.csv"))
      
      result <- read_plate(i, "wells", filename)
      expect_that(result$full, is_identical_to(getWellIds(i)))
      expect_that(result$full, is_identical_to(result$wells))
      r <- is.na(result$partial) | result$partial == as.character(result$wells)
      expect_that(all(r), is_true())
   })
}
