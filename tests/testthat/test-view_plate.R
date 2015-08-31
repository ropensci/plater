################################################################################
context("testing view_plate-view_plate (96 well)")
################################################################################
checkRow <- function(result, rowNumber, startLetter, endLetter, append = NA) {
   result <- as.character(unlist(result[rowNumber, ]))
   expected <- (letters)[startLetter:endLetter]
   if (!missing(append)) {
      expected <- c(expected, append)
   }
   #print(paste0("row number = ", rowNumber, " ", result == expected))
   expect_that(result, 
      is_equivalent_to(expected))
}

for (i in c(24, 48, 96, 384)) {
   cols <- numberOfColumns(i)
   rows <- numberOfRows(i)
   odds <- seq(1, rows, by = 2)
   evens <- seq(2, rows + 1, by = 2)
   colStr <- if (cols < 9) colStr <- paste0(0, cols) else as.character(cols)
   maxLetters <- cols * 2
   if (i == 384) {
      maxLetters <- 24
      odds <- 1:rows
      evens <- c()
   }
   
   test_that("full plate displays correct results", {
      d <- data.frame(wells = getWellIds(i), d = letters[1:maxLetters])
      result <- view_plate(i, d, "wells", "d")
      sapply(odds, FUN = function(i) checkRow(result, i, 1, cols))
      sapply(evens, FUN = function(i) checkRow(result, i, cols + 1, 2 * cols))
   })
   
   test_that("missing column displays correct results", {
      d <- data.frame(wells = getWellIds(i), d = letters[1:maxLetters])
      d <- d[!(substr(d$wells, 2, 3) == colStr), ]
      result <- view_plate(i, d, "wells", "d")
      sapply(odds, FUN = function(i) checkRow(result, i, 1, (cols - 1), "."))
      sapply(evens, FUN = function(i) 
         checkRow(result, i, cols + 1, 2 * cols - 1, "."))
   })
   
   test_that("missing row displays correct results", {
      d <- data.frame(wells = getWellIds(i), d = letters[1:maxLetters])
      d <- d[!(substr(d$wells, 1, 1) == "A"), ]
      result <- view_plate(i, d, "wells", "d")
      checkRow(result, 1, 0, 0, rep(".", cols))   
      sapply(odds[2:length(odds)], FUN = function(i) checkRow(result, i, 1, cols))
      sapply(evens, FUN = function(i) checkRow(result, i, (cols + 1), (2 * cols)))
   })
   
   # I found that if a data frame is supplied with well IDs that are factors and
   # the levels of the well IDs are not alphabetically sorted, it would throw an
   # invalid well IDs error (and if that was ignored, sort display the wrong 
   # wells in the wrong places). This is because it would sort by the order of  
   # the levels, not alphabetically. This is a regression test for that bug. 
   test_that("well IDs as factors with weirdly ordered levels are display correctly", {
      wellIds <- getWellIds(i)
      wellIds <- factor(wellIds, levels = c(wellIds[(i/2+1):i], wellIds[1:(i/2)]))
      
      d <- data.frame(wells = wellIds, d = letters[1:maxLetters])
      result <- view_plate(i, d, "wells", "d")
      sapply(odds, FUN = function(i) checkRow(result, i, 1, cols))
      sapply(evens, FUN = function(i) checkRow(result, i, cols + 1, 2 * cols))
   })
}


################################################################################
context("testing view_plate-ensureCorrectWellIds")
################################################################################
for (i in c(12, 24, 48, 96, 384)) {  
   wells <- getWellIds(i)
   test_that("ensureCorrectWellIds stops if more wells than plate size", {
      expect_that(ensureCorrectWellIds(data.frame(x = 1:97), "x", 96), 
         throws_error())
   })
   
   test_that("ensureCorrectWellIds returns identical valid input", {
      temp <- data.frame(Ids = wells)
      expect_that(ensureCorrectWellIds(temp, "Ids", i), 
         is_identical_to(temp))
   })
}
################################################################################
context("testing view_plate-areWellIdsCorrect()")
################################################################################
for (i in c(12, 24, 48, 96, 384)) { 
   wells <- getWellIds(i)
   test_that("areWellIdsCorrect() returns false for too short or long", {
      expect_that(areWellIdsCorrect(wells[1:(i - 1)], i), is_false())
      expect_that(areWellIdsCorrect("A01", i), is_false())   
      expect_that(areWellIdsCorrect(c(wells, "H13"), i), is_false()) 
   })
   
   test_that("areWellIdsCorrect() returns false for repeated wells", {
      expect_that(areWellIdsCorrect(c(wells[1:i - 1], "A01"), i), is_false())
   })
   
   test_that("areWellIdsCorrect() returns false for missing leading zeros", {
      expect_that(areWellIdsCorrect(c(paste0("A", 1:4), wells[1:4]), i), 
         is_false())
   })
   
   test_that("areWellIdsCorrect() returns true for correct input", {
      expect_that(areWellIdsCorrect(wells, i), is_true())
   })
   
   test_that("areWellIdsCorrect() returns true for correct but out of order", {
      expect_that(areWellIdsCorrect(wells[sample(1:i, i)], i), is_true())
   })
}
################################################################################
context("testing view_plate-fillInMissingWellIds()")
################################################################################
for (i in c(12, 24, 48, 96, 384)) { 
   # set up data frame
   if (i == 12) subset = 12 else subset = 24
   text <- paste0(letters[1:subset], LETTERS[26:(26 - subset + 1)])
   numbers <- sample(1:1000, i)
   data <- data.frame(wells = getWellIds(i), text = text, numbers = numbers)
   
   # helper function
   validate <- function(result, data, bounds) {
      expect_that(result$wells, is_identical_to(data$wells))
      expect_that(result[0:bounds, ], is_identical_to(data[0:bounds, ]))
      expect_that(colnames(result), is_identical_to(colnames(data)))
   }
   
   test_that("fillInMissingWellIds() fails with equal or more rows than wells", {
      expect_that(fillInMissingWellIds(data, "wells", i), throws_error())
      expect_that(fillInMissingWellIds(rbind(data, data), "wells", i), 
         throws_error())
   })
   
   test_that("fillInMissingWellIds() works with three columns as factor", {
      result <- fillInMissingWellIds(data[1:10, ], "wells", i)
      validate(result, data, 10)
   })
   
   test_that("fillInMissingWellIds() works with three columns as character", {
      d <- data
      d$wells <- as.character(d$wells)
      result <- fillInMissingWellIds(d[1:10, ], "wells", i)
      validate(result, d, 10)
   })
   
   test_that("fillInMissingWellIds() works with only well column", {
      # use drop = FALSE to prevent data frame becoming vector
      result <- fillInMissingWellIds(data[1:10, "wells", drop = FALSE], "wells", i)
      validate(result, data[ , "wells", drop = FALSE], 10)
   })
      
   test_that("fillInMissingWellIds() works with zero rows, one columns", {
      result <- fillInMissingWellIds(data[0, "wells", drop = FALSE], "wells", i)
      expect_that(result$wells, is_identical_to(data$wells))
      expect_that(colnames(result), is_identical_to("wells"))   
   })
   
   test_that("fillInMissingWellIds() throws error with missing leading zeroes", {
      d <- data
      d$wells <- getWellIdsWithoutLeadingZeroes(i)
      expect_that(fillInMissingWellIds(d, "wells", i), throws_error())
   })
   
   test_that("fillInMissingWellIds() doesn't change if well IDs are invalid", {
      d <- data
      if (i == 12) subset = 12 else subset = 24
      d$wells <- letters[1:subset]
      subset <- 10
      result <- fillInMissingWellIds(d[1:subset, ], "wells", i)
      validate(result[1:subset, ], d[1:subset, ], subset)
   })
}

################################################################################
context("testing view_plate-correctLeadingZeroes()")
################################################################################
for (i in c(12, 24, 48, 96, 384)) { 
   with <- data.frame(w = getWellIds(i), b = 1:i)
   without <- data.frame(w = getWellIdsWithoutLeadingZeroes(i), b = 1:i)
   
   test_that("correctLeadingZeroes returns same df for correct wells", {
      expect_that(correctLeadingZeroes(with, "w", i), is_identical_to(with))   
   })
   
   test_that("correctLeadingZeroes returns same df for correct wells as character", {
      d <- with
      d$w <- as.character(d$w)
      expect_that(correctLeadingZeroes(d, "w", i), is_identical_to(d))   
   })
   
   test_that("correctLeadingZeroes doesn't change unrelated text", {
      d <- data.frame(w = letters[1:24], b = 1:i)
      expect_that(correctLeadingZeroes(d, "w", i), is_identical_to(d))   
   })
   
   test_that("correctLeadingZeroes corrects incorrect wells", {
      expect_that(correctLeadingZeroes(without, "w", i), is_identical_to(with))
   })
   
   test_that("correctLeadingZeroes corrects incorrect wells as char", {
      correct <- with
      correct$w <- as.character(correct$w)
      d <- without
      d$w <- as.character(d$w)
      expect_that(correctLeadingZeroes(d, "w", i), 
         is_identical_to(correct))
   })
   
   test_that("correctLeadingZeroes fixes one incorrect well", {
      d <- with
      d$w <- factor(d$w, levels = c(levels(d$w), "A1"))
      d[1, "w"] <- "A1"
      expect_that(correctLeadingZeroes(d, "w", i), is_identical_to(with))   
   })
   
   test_that("correctLeadingZeroes throws error with incorrect plate size", {
      expect_that(correctLeadingZeroes(with, "w", 95), throws_error())   
   })
}
################################################################################
context("testing view_plate-areLeadingZeroesValid()")
################################################################################
for (i in c(12, 24, 48, 96, 384)) { 
   with <- data.frame(w = getWellIds(i))
   without <- data.frame(w = getWellIdsWithoutLeadingZeroes(i))
   
   test_that("areLeadingZeroesValid returns TRUE for correct wells", {
      expect_that(areLeadingZeroesValid(with, "w", i), is_true())   
   })
   
   test_that("areLeadingZeroesValid returns TRUE for correct wells as char", {
      d <- with
      d$w <- as.character(d$w)
      expect_that(areLeadingZeroesValid(d, "w", i), is_true())   
   })
   
   test_that("areLeadingZeroesValid returns TRUE for unrelated text", {
      d <- data.frame(w = 1:i)
      expect_that(areLeadingZeroesValid(d, "w", i), is_true())   
   })
   
   test_that("areLeadingZeroesValid returns FALSE for incorrect wells", {
      expect_that(areLeadingZeroesValid(without, "w", i), is_false())   
   })
   
   test_that("areLeadingZeroesValid returns FALSE for incorrect wells as char", {
      d <- without
      d$w <- as.character(d$w)
      expect_that(areLeadingZeroesValid(d, "w", i), is_false())   
   })
   
   test_that("areLeadingZeroesValid returns FALSE for one incorrect well", {
      d <- with
      d$w <- factor(d$w, levels = c(levels(d$w), "A1"))
      d[1, "w"] <- "A1"
      expect_that(areLeadingZeroesValid(d, "w", i), is_false())   
   })
}