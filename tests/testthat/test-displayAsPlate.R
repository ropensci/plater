require(testthat)
path <- "testData/"

wells <- getWellIds(96)

################################################################################
context("testing displayAsPlate-ensureCorrectWellIds")
################################################################################
test_that("ensureCorrectWellIds stops if more wells than plate size", {
   expect_that(ensureCorrectWellIds(data.frame(x = 1:97), "x", 96), 
      throws_error())
})

test_that("ensureCorrectWellIds returns true for valid input", {
   expect_that(ensureCorrectWellIds(data.frame(Ids = wells), "Ids", 96), 
      is_true())
})

################################################################################
context("testing displayAsPlate-areWellIdsCorrect()")
################################################################################
test_that("areWellIdsCorrect() returns false for too short or long", {
   expect_that(areWellIdsCorrect(wells[1:95], 96), is_false())
   expect_that(areWellIdsCorrect("A01", 96), is_false())   
   expect_that(areWellIdsCorrect(c(wells, "H13"), 96), is_false()) 
})

test_that("areWellIdsCorrect() returns false for repeated wells", {
   expect_that(areWellIdsCorrect(c(wells[1:95], "A01"), 96), is_false())
})

test_that("areWellIdsCorrect() returns false for missing leading zeros", {
   expect_that(areWellIdsCorrect(c(paste0("A", 1:9), wells[10:96]), 96), 
      is_false())
})

test_that("areWellIdsCorrect() returns true for correct input", {
   expect_that(areWellIdsCorrect(wells, 96), is_true())
})

test_that("areWellIdsCorrect() returns true for correct but out of order", {
   expect_that(areWellIdsCorrect(wells[sample(1:96, 96)], 96), is_true())
})


################################################################################
context("testing displayAsPlate-fillInMissingWellIds()")
################################################################################
# test with missing leading zeros
# test with no missing wells
# test with invalid well IDs
# test with well ids as factor and as character
# test with no other columns than well ids
# test with many other columns

data <- data.frame(wells = getWellIds(96)[sample(1:96, 24)], first = sample(1:96, 24), 
   second = sample(1:96, 24))
wellIdColumn <- "wells"
plateSize <- 96




# TODO
