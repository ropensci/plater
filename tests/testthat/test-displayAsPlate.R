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

test_that("ensureCorrectWellIds returns valid input", {
   temp <- data.frame(Ids = wells)
   expect_that(ensureCorrectWellIds(temp, "Ids", 96), 
      is_identical_to(temp))
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
# set up data frame
text <- paste0(letters[1:24], LETTERS[26:3])
numbers <- sample(1:1000, 96)
data <- data.frame(wells = getWellIds(96), text = text, numbers = numbers)

# helper function
validate <- function(result, data, bounds) {
   expect_that(result$wells, is_identical_to(data$wells))
   expect_that(result[0:bounds, ], is_identical_to(data[0:bounds, ]))
   expect_that(colnames(result), is_identical_to(colnames(data)))
}

test_that("fillInMissingWellIds() fails with equal or more rows than wells", {
   expect_that(fillInMissingWellIds(data, "wells", 96), throws_error())
   expect_that(fillInMissingWellIds(rbind(data, data), "wells", 96), 
      throws_error())
})

test_that("fillInMissingWellIds() works with three columns", {
   result <- fillInMissingWellIds(data[1:10, ], "wells", 96)
   validate(result, data, 10)
})

test_that("fillInMissingWellIds() works with only well column", {
   # use drop = FALSE to prevent data frame becoming vector
   result <- fillInMissingWellIds(data[1:10, "wells", drop = FALSE], "wells", 96)
   validate(result, data[ , "wells", drop = FALSE], 10)
})

test_that("fillInMissingWellIds() works with zero rows, three columns", {
   result <- fillInMissingWellIds(data[0, ], "wells", 96)
   expect_that(result$wells, is_identical_to(data$wells))
   
   # if a data frame with no rows is put it, the column order is different from 
   # expected: it orders according to internal processing ("temp"), not the
   # order in the input data frame
   expect_that(colnames(result), is_identical_to(colnames(data)))   
   expect_that(sum(is.na(result$text)), equals(96))
   expect_that(sum(is.na(result$numbers)), equals(96))
})

test_that("fillInMissingWellIds() works with zero rows, one columns", {
   result <- fillInMissingWellIds(data[0, "wells", drop = FALSE], "wells", 96)
   expect_that(result$wells, is_identical_to(data$wells))
   expect_that(colnames(result), is_identical_to("wells"))   
})

# test with missing leading zeros
# test with invalid well IDs
# test with well ids as factor and as character


test_that("todo", {
   expect_that(TRUE, is_false())
})



# TODO




################################################################################
context("testing displayAsPlate-correctLeadingZeroes()")
################################################################################
   
   # test with no missing leading zeroes
   # test with leading zeroes missing
   # test with invalid well IDs
   # test with incorrect plateSize
   # test with well ids as factor and as character
test_that("todo", {
   expect_that(TRUE, is_false())
})