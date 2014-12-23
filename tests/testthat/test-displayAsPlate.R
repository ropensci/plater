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

test_that("fillInMissingWellIds() works with three columns as factor", {
   result <- fillInMissingWellIds(data[1:10, ], "wells", 96)
   validate(result, data, 10)
})

test_that("fillInMissingWellIds() works with three columns as character", {
   d <- data
   d$wells <- as.character(d$wells)
   result <- fillInMissingWellIds(d[1:10, ], "wells", 96)
   validate(result, d, 10)
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

test_that("fillInMissingWellIds() throws error with missing leading zeroes", {
   d <- data
   d$wells <- getWellIdsWithoutLeadingZeroes(96)
   expect_that(fillInMissingWellIds(d, "wells", 96), throws_error())
})

test_that("fillInMissingWellIds() doesn't change if well IDs are invalid", {
   d <- data
   d$wells <- letters[1:24]
   result <- fillInMissingWellIds(d[1:24, ], "wells", 96)
   validate(result[1:24, ], d[1:24, ], 24)
})

################################################################################
context("testing displayAsPlate-correctLeadingZeroes()")
################################################################################
with <- data.frame(w = getWellIds(96), b = 1:96)
without <- data.frame(w = getWellIdsWithoutLeadingZeroes(96), b = 1:96)

test_that("correctLeadingZeroes returns same df for correct wells", {
   expect_that(correctLeadingZeroes(with, "w", 96), is_identical_to(with))   
})

test_that("correctLeadingZeroes returns same df for correct wells as character", {
   d <- with
   d$w <- as.character(d$w)
   expect_that(correctLeadingZeroes(d, "w", 96), is_identical_to(d))   
})

test_that("correctLeadingZeroes doesn't change unrelated text", {
   d <- data.frame(w = letters[1:24], b = 1:96)
   expect_that(correctLeadingZeroes(d, "w", 96), is_identical_to(d))   
})

test_that("correctLeadingZeroes corrects incorrect wells", {
   expect_that(correctLeadingZeroes(without, "w", 96), is_identical_to(with))
})

test_that("correctLeadingZeroes corrects incorrect wells as char", {
   correct <- with
   correct$w <- as.character(correct$w)
   d <- without
   d$w <- as.character(d$w)
   expect_that(correctLeadingZeroes(d, "w", 96), 
      is_identical_to(correct))
})

test_that("correctLeadingZeroes fixes one incorrect well", {
   d <- with
   d$w <- factor(d$w, levels = c(levels(d$w), "A1"))
   d[1, "w"] <- "A1"
   expect_that(correctLeadingZeroes(d, "w", 96), is_identical_to(with))   
})

test_that("correctLeadingZeroes throws error with incorrect plate size", {
   expect_that(correctLeadingZeroes(with, "w", 95), throws_error())   
})

################################################################################
context("testing displayAsPlate-areLeadingZeroesValid()")
################################################################################
with <- data.frame(w = getWellIds(96))
without <- data.frame(w = getWellIdsWithoutLeadingZeroes(96))

test_that("areLeadingZeroesValid returns TRUE for correct wells", {
   expect_that(areLeadingZeroesValid(with, "w", 96), is_true())   
})

test_that("areLeadingZeroesValid returns TRUE for correct wells as char", {
   d <- with
   d$w <- as.character(d$w)
   expect_that(areLeadingZeroesValid(d, "w", 96), is_true())   
})

test_that("areLeadingZeroesValid returns TRUE for unrelated text", {
   d <- data.frame(w = 1:96)
   expect_that(areLeadingZeroesValid(d, "w", 96), is_true())   
})

test_that("areLeadingZeroesValid returns FALSE for incorrect wells", {
   expect_that(areLeadingZeroesValid(without, "w", 96), is_false())   
})

test_that("areLeadingZeroesValid returns FALSE for incorrect wells as char", {
   d <- without
   d$w <- as.character(d$w)
   expect_that(areLeadingZeroesValid(d, "w", 96), is_false())   
})

test_that("areLeadingZeroesValid returns FALSE for one incorrect well", {
   d <- with
   d$w <- factor(d$w, levels = c(levels(d$w), "A1"))
   d[1, "w"] <- "A1"
   expect_that(areLeadingZeroesValid(d, "w", 96), is_false())   
})