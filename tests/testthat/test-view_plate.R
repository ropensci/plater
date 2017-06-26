################################################################################
context("testing view_plate-view_plate (96 well)")
################################################################################
checkRow <- function(result, rowNumber, startLetter, endLetter, append = NA) {
   result <- as.character(unlist(result[rowNumber, ]))
   expected <- (letters)[startLetter:endLetter]
   if (!missing(append)) {
      expected <- c(expected, append)
   }
   expect_that(result, 
      is_equivalent_to(expected))
}

for (i in c(24, 48, 96, 384)) {
   cols <- number_of_columns(i)
   rows <- number_of_rows(i)
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
      d <- data.frame(wells = get_well_ids(i), d = letters[1:maxLetters])
      result <- view_plate(d, "wells", "d", i)[[1]]
      sapply(odds, FUN = function(i) checkRow(result, i, 1, cols))
      sapply(evens, FUN = function(i) checkRow(result, i, cols + 1, 2 * cols))
   })
   
   test_that("missing column displays correct results", {
      d <- data.frame(wells = get_well_ids(i), d = letters[1:maxLetters])
      d <- d[!(substr(d$wells, 2, 3) == colStr), ]
      result <- view_plate(d, "wells", "d", i)[[1]]
      sapply(odds, FUN = function(i) checkRow(result, i, 1, (cols - 1), "."))
      sapply(evens, FUN = function(i) 
         checkRow(result, i, cols + 1, 2 * cols - 1, "."))
   })
   
   test_that("missing row displays correct results", {
      d <- data.frame(wells = get_well_ids(i), d = letters[1:maxLetters])
      d <- d[!(substr(d$wells, 1, 1) == "A"), ]
      result <- view_plate(d, "wells", "d", i)[[1]]
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
      wellIds <- get_well_ids(i)
      wellIds <- factor(wellIds, levels = c(wellIds[(i/2+1):i], wellIds[1:(i/2)]))
      
      d <- data.frame(wells = wellIds, d = letters[1:maxLetters])
      result <- view_plate(d, "wells", "d", i)[[1]]
      sapply(odds, FUN = function(i) checkRow(result, i, 1, cols))
      sapply(evens, FUN = function(i) checkRow(result, i, cols + 1, 2 * cols))
   })
}


################################################################################
context("testing view_plate-ensure_correct_well_ids")
################################################################################
for (i in c(12, 24, 48, 96, 384)) {  
   wells <- get_well_ids(i)
   test_that("ensure_correct_well_ids stops if more wells than plate size", {
      expect_that(ensure_correct_well_ids(data.frame(x = 1:97), "x", 96), 
         throws_error())
   })
   
   test_that("ensure_correct_well_ids returns identical valid input", {
      temp <- data.frame(Ids = wells)
      expect_that(ensure_correct_well_ids(temp, "Ids", i), 
         is_identical_to(temp))
   })
}
################################################################################
context("testing view_plate-are_well_ids_correct()")
################################################################################
for (i in c(12, 24, 48, 96, 384)) { 
   wells <- get_well_ids(i)
   test_that("are_well_ids_correct() returns false for too short or long", {
      expect_that(are_well_ids_correct(wells[1:(i - 1)], i), is_false())
      expect_that(are_well_ids_correct("A01", i), is_false())   
      expect_that(are_well_ids_correct(c(wells, "H13"), i), is_false()) 
   })
   
   test_that("are_well_ids_correct() returns false for repeated wells", {
      expect_that(are_well_ids_correct(c(wells[1:i - 1], "A01"), i), is_false())
   })
   
   test_that("are_well_ids_correct() returns false for missing leading zeros", {
      expect_that(are_well_ids_correct(c(paste0("A", 1:4), wells[1:4]), i), 
         is_false())
   })
   
   test_that("are_well_ids_correct() returns true for correct input", {
      expect_that(are_well_ids_correct(wells, i), is_true())
   })
   
   test_that("are_well_ids_correct() returns true for correct but out of order", {
      expect_that(are_well_ids_correct(wells[sample(1:i, i)], i), is_true())
   })
}
################################################################################
context("testing view_plate-fill_in_missing_well_ids()")
################################################################################
for (i in c(12, 24, 48, 96, 384)) { 
   # set up data frame
   if (i == 12) subset = 12 else subset = 24
   text <- paste0(letters[1:subset], LETTERS[26:(26 - subset + 1)])
   numbers <- sample(1:1000, i)
   data <- data.frame(wells = get_well_ids(i), text = text, numbers = numbers)
   
   # helper function
   validate <- function(result, data, bounds) {
      expect_that(result$wells, is_identical_to(data$wells))
      expect_that(result[0:bounds, ], is_identical_to(data[0:bounds, ]))
      expect_that(colnames(result), is_identical_to(colnames(data)))
   }
   
   test_that("fill_in_missing_well_ids() fails with equal or more rows than wells", {
      expect_that(fill_in_missing_well_ids(data, "wells", i), throws_error())
      expect_that(fill_in_missing_well_ids(rbind(data, data), "wells", i), 
         throws_error())
   })
   
   test_that("fill_in_missing_well_ids() works with three columns as factor", {
      result <- fill_in_missing_well_ids(data[1:10, ], "wells", i)
      validate(result, data, 10)
   })
   
   test_that("fill_in_missing_well_ids() works with three columns as character", {
      d <- data
      d$wells <- as.character(d$wells)
      result <- fill_in_missing_well_ids(d[1:10, ], "wells", i)
      validate(result, d, 10)
   })
   
   test_that("fill_in_missing_well_ids() works with only well column", {
      # use drop = FALSE to prevent data frame becoming vector
      result <- fill_in_missing_well_ids(data[1:10, "wells", drop = FALSE], "wells", i)
      validate(result, data[ , "wells", drop = FALSE], 10)
   })
      
   test_that("fill_in_missing_well_ids() works with zero rows, one columns", {
      result <- fill_in_missing_well_ids(data[0, "wells", drop = FALSE], "wells", i)
      expect_that(result$wells, is_identical_to(data$wells))
      expect_that(colnames(result), is_identical_to("wells"))   
   })
   
   test_that("fill_in_missing_well_ids() throws error with missing leading zeroes", {
      d <- data
      d$wells <- get_well_ids_without_leading_zeroes(i)
      expect_that(fill_in_missing_well_ids(d, "wells", i), throws_error())
   })
   
   test_that("fill_in_missing_well_ids() doesn't change if well IDs are invalid", {
      d <- data
      if (i == 12) subset = 12 else subset = 24
      d$wells <- letters[1:subset]
      subset <- 10
      result <- fill_in_missing_well_ids(d[1:subset, ], "wells", i)
      validate(result[1:subset, ], d[1:subset, ], subset)
   })
}

################################################################################
context("testing view_plate-correct_leading_zeroes()")
################################################################################
for (i in c(12, 24, 48, 96, 384)) { 
   with <- data.frame(w = get_well_ids(i), b = 1:i)
   without <- data.frame(w = get_well_ids_without_leading_zeroes(i), b = 1:i)
   
   test_that("correct_leading_zeroes returns same df for correct wells", {
      expect_that(correct_leading_zeroes(with, "w", i), is_identical_to(with))   
   })
   
   test_that("correct_leading_zeroes returns same df for correct wells as character", {
      d <- with
      d$w <- as.character(d$w)
      expect_that(correct_leading_zeroes(d, "w", i), is_identical_to(d))   
   })
   
   test_that("correct_leading_zeroes doesn't change unrelated text", {
      d <- data.frame(w = letters[1:24], b = 1:i)
      expect_that(correct_leading_zeroes(d, "w", i), is_identical_to(d))   
   })
   
   test_that("correct_leading_zeroes corrects incorrect wells", {
      expect_that(correct_leading_zeroes(without, "w", i), is_identical_to(with))
   })
   
   test_that("correct_leading_zeroes corrects incorrect wells as char", {
      correct <- with
      correct$w <- as.character(correct$w)
      d <- without
      d$w <- as.character(d$w)
      expect_that(correct_leading_zeroes(d, "w", i), 
         is_identical_to(correct))
   })
   
   test_that("correct_leading_zeroes fixes one incorrect well", {
      d <- with
      d$w <- factor(d$w, levels = c(levels(d$w), "A1"))
      d[1, "w"] <- "A1"
      expect_that(correct_leading_zeroes(d, "w", i), is_identical_to(with))   
   })
   
   test_that("correct_leading_zeroes throws error with incorrect plate size", {
      expect_that(correct_leading_zeroes(with, "w", 95), throws_error())   
   })
}
################################################################################
context("testing view_plate-are_leading_zeroes_valid()")
################################################################################
for (i in c(12, 24, 48, 96, 384)) { 
   with <- data.frame(w = get_well_ids(i))
   without <- data.frame(w = get_well_ids_without_leading_zeroes(i))
   
   test_that("are_leading_zeroes_valid returns TRUE for correct wells", {
      expect_that(are_leading_zeroes_valid(with, "w", i), is_true())   
   })
   
   test_that("are_leading_zeroes_valid returns TRUE for correct wells as char", {
      d <- with
      d$w <- as.character(d$w)
      expect_that(are_leading_zeroes_valid(d, "w", i), is_true())   
   })
   
   test_that("are_leading_zeroes_valid returns TRUE for unrelated text", {
      d <- data.frame(w = 1:i)
      expect_that(are_leading_zeroes_valid(d, "w", i), is_true())   
   })
   
   test_that("are_leading_zeroes_valid returns FALSE for incorrect wells", {
      expect_that(are_leading_zeroes_valid(without, "w", i), is_false())   
   })
   
   test_that("are_leading_zeroes_valid returns FALSE for incorrect wells as char", {
      d <- without
      d$w <- as.character(d$w)
      expect_that(are_leading_zeroes_valid(d, "w", i), is_false())   
   })
   
   test_that("are_leading_zeroes_valid returns FALSE for one incorrect well", {
      d <- with
      d$w <- factor(d$w, levels = c(levels(d$w), "A1"))
      d[1, "w"] <- "A1"
      expect_that(are_leading_zeroes_valid(d, "w", i), is_false())   
   })
}

test_that("view_plate works on grouped tibbles", {
    data <- dplyr::tibble(well = c("A01", "F12", "G09"), Droplets = 1:3)
    data <- dplyr::group_by(data, Droplets)
    
    # NA means expect no error
    expect_error(view_plate(data, "well", "Droplets", 96), NA)
    
    data <- dplyr::tibble(well = c("A01", "F12", "G09"), Droplets = 1:3)
    data <- dplyr::group_by(data, well)
    
    expect_error(view_plate(data, "well", "Droplets", 96), NA)
})