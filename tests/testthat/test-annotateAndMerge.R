
################################################################################
context("testing annotateAndMerge-annotateAndMerge96WellPlate()")
################################################################################

test_that("annotateAndMerge96WellPlate works for complete valid data", {
   filename <- "testData/allWellIds.csv"
   complete <- data.frame(wells = getWellIds(96), d = letters[1:24])
   complete$d <- as.character(complete$d)

   result <- annotateAndMerge96WellPlate(complete, "wells", filename, "values")
   expect_that(result$values, is_identical_to(getWellIds(96)))
   expect_that(factor(result$values), is_identical_to(result$wells))
})

test_that("annotateAndMerge96WellPlate works without leading zeroes", {
   filename <- "testData/allWellIds.csv"
   complete <- data.frame(wells = getWellIdsWithoutLeadingZeroes(96), 
      d = letters[1:24])
   complete$d <- as.character(complete$d)
   
   result <- annotateAndMerge96WellPlate(complete, "wells", filename, "values")
   expect_that(result$wells, 
      is_identical_to(factor(getWellIdsWithoutLeadingZeroes(96))))
   expect_that(factor(removeLeadingZeroes(result$values)), 
      is_identical_to(result$wells))
})

test_that("annotateAndMerge96WellPlate works for missing data from plate", {
   filename <- "testData/wellIdsAndEmptyWells.csv"
   complete <- data.frame(wells = getWellIds(96), d = letters[1:24])
   complete$d <- as.character(complete$d)
   
   result <- annotateAndMerge96WellPlate(complete, "wells", filename, "values")
   
   expect_that(as.character(result$wells), is_identical_to(getWellIds(96)))
   r <- is.na(result$values) | result$values == as.character(result$wells)
   expect_that(all(r), is_true())
})

test_that(paste("annotateAndMerge96WellPlate works for missing data from",
      "plate and no leading zeroes"), {
   filename <- "testData/wellIdsAndEmptyWells.csv"
   complete <- data.frame(wells = getWellIdsWithoutLeadingZeroes(96), 
      d = letters[1:24])
   complete$d <- as.character(complete$d)
   
   result <- annotateAndMerge96WellPlate(complete, "wells", filename, "values")
   
   expect_that(result$wells, 
      is_identical_to(factor(getWellIdsWithoutLeadingZeroes(96))))
   result$values <- removeLeadingZeroes(result$values)
   r <- is.na(result$values) | result$values == as.character(result$wells)
   expect_that(all(r), is_true())
})

test_that("annotateAndMerge96WellPlate stops if wells missing from df", {
   filename <- "testData/allWellIds.csv"
   complete <- data.frame(wells = getWellIds(96), d = letters[1:24])
   complete$d <- as.character(complete$d)
   
   expect_that(annotateAndMerge96WellPlate(complete[1:95, ], "wells", 
      filename, "values"),
      throws_error())
})

test_that(paste("annotateAndMerge96WellPlate stops if some wells are missing",
   "leading zeroes and some aren't"), {
   filename <- "testData/allWellIds.csv"
   complete <- data.frame(wells = getWellIds(96), d = letters[1:24])
   complete$d <- as.character(complete$d)
   set.seed(10)
   complete$wells <- ifelse(rnorm(1) < 0, 
      removeLeadingZeroes(complete$wells), 
      complete$wells)
   
   expect_that(annotateAndMerge96WellPlate(complete, "wells", 
      filename, "values"),
      throws_error())
})

################################################################################
context("testing annotateAndMerge-wrongWellsErrorMessage()")
################################################################################
test_that("wrongWellsErrorMessage returns correct message with 1 well missing", {
   filename <- "testData/allWellIds.csv"
   complete <- data.frame(wells = getWellIds(96), d = letters[1:24])
   complete$d <- as.character(complete$d)
   
   annotations <- annotate96WellPlate(filename, "values")
   annotations <- annotations[!(is.na(annotations$values)), ]
   
   message <- wrongWellsErrorMessage(complete[1:95, ], "wells", annotations)
   expected <- paste0("Some wells in your file are not in the data frame ",
      "you provided, but they all should be. The missing wells are: H12.")
   expect_that(message, is_identical_to(expected))
})

test_that("wrongWellsErrorMessage returns correct message with 2 wells missing", {
   filename <- "testData/allWellIds.csv"
   complete <- data.frame(wells = getWellIds(96), d = letters[1:24])
   complete$d <- as.character(complete$d)
   
   annotations <- annotate96WellPlate(filename, "values")
   annotations <- annotations[!(is.na(annotations$values)), ]
   
   message <- wrongWellsErrorMessage(complete[1:94, ], "wells", annotations)
   expected <- paste0("Some wells in your file are not in the data frame ",
      "you provided, but they all should be. The missing wells are: H11, H12.")
   expect_that(message, is_identical_to(expected))
})

test_that("wrongWellsErrorMessage returns correct message with 95 wells missing", {
   filename <- "testData/allWellIds.csv"
   complete <- data.frame(wells = getWellIds(96), d = letters[1:24])
   complete$d <- as.character(complete$d)
   
   annotations <- annotate96WellPlate(filename, "values")
   annotations <- annotations[!(is.na(annotations$values)), ]
   
   message <- wrongWellsErrorMessage(complete[1, ], "wells", annotations)
   expected <- paste0("Some wells in your file are not in the data frame ",
      "you provided, but they all should be. The missing wells are: ", 
      "A02, A03, A04, A05, A06, A07, A08, A09, A10, A11, A12, B01, B02, B03, ",
      "B04, B05, B06, B07, B08, B09, B10, B11, B12, C01, C02, C03, C04, C05, ", 
      "C06, C07, C08, C09, C10, C11, C12, D01, D02, D03, D04, D05, D06, D07, ", 
      "D08, D09, D10, D11, D12, E01, E02, E03, E04, E05, E06, E07, E08, E09, ", 
      "E10, E11, E12, F01, F02, F03, F04, F05, F06, F07, F08, F09, F10, F11, ",
      "F12, G01, G02, G03, G04, G05, G06, G07, G08, G09, G10, G11, G12, H01, ",
      "H02, H03, H04, H05, H06, H07, H08, H09, H10, H11, H12", ".")
   expect_that(message, is_identical_to(expected))
})

test_that("wrongWellsErrorMessage errors with valid input", {
   filename <- "testData/allWellIds.csv"
   complete <- data.frame(wells = getWellIds(96), d = letters[1:24])
   complete$d <- as.character(complete$d)
   
   annotations <- annotate96WellPlate(filename, "values")
   annotations <- annotations[!(is.na(annotations$values)), ]
   
   expect_that(wrongWellsErrorMessage(complete, "wells", annotations), 
      throws_error())
})