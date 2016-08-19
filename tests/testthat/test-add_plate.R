for (i in c(12, 24, 48, 96, 384)) {
   path <- paste0("testData/", i, "/")
   ################################################################################
   context("testing add_plate-add_plate()")
   ################################################################################
   n_letters <- ifelse(i == 12, 12, 24)
   
   test_that("add_plate works for complete valid data", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = get_well_ids(i), d = letters[1:n_letters])
      complete$d <- as.character(complete$d)
   
      result <- add_plate(complete, filename, "wells")
      expect_that(result$values, is_identical_to(get_well_ids(i)))
      expect_that(factor(result$values), is_identical_to(result$wells))
   })
   
   test_that("add_plate works with reversed file and data arguments but gives warning", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = get_well_ids(i), d = letters[1:n_letters])
      complete$d <- as.character(complete$d)
      
      expect_warning({
         result <- add_plate(filename, complete, "wells")
         expect_that(result$values, is_identical_to(get_well_ids(i)))
         expect_that(factor(result$values), is_identical_to(result$wells))         
      }, "reversed")
   })
   
   test_that("add_plate works without leading zeroes", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = get_well_ids_without_leading_zeroes(i), 
         d = letters[1:n_letters])
      complete$d <- as.character(complete$d)
      
      result <- add_plate(complete, filename, "wells")
      expect_that(result$wells, 
         is_identical_to(factor(get_well_ids_without_leading_zeroes(i))))
      expect_that(factor(remove_leading_zeroes(result$values)), 
         is_identical_to(result$wells))
   })
   
   test_that("add_plate works for missing data from plate", {
      filename <- paste0(path, "wellIdsAndEmptyWells.csv")
      complete <- data.frame(wells = get_well_ids(i), d = letters[1:n_letters])
      complete$d <- as.character(complete$d)
      
      result <- add_plate(complete, filename, "wells")
      
      expect_that(as.character(result$wells), is_identical_to(get_well_ids(i)))
      r <- is.na(result$values) | result$values == as.character(result$wells)
      expect_that(all(r), is_true())
   })
   
   test_that(paste("add_plate works for missing data from",
         "plate and no leading zeroes"), {
      filename <- paste0(path, "wellIdsAndEmptyWells.csv")
      complete <- data.frame(wells = get_well_ids_without_leading_zeroes(i), 
         d = letters[1:n_letters])
      complete$d <- as.character(complete$d)
      
      result <- add_plate(complete, filename, "wells")
      
      expect_that(result$wells, 
         is_identical_to(factor(get_well_ids_without_leading_zeroes(i))))
      result$values <- remove_leading_zeroes(result$values)
      r <- is.na(result$values) | result$values == as.character(result$wells)
      expect_that(all(r), is_true())
   })
   
   test_that("add_plate stops if wells missing from df", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = get_well_ids(i), d = letters[1:n_letters])
      complete$d <- as.character(complete$d)
      
      expect_that(add_plate(complete[1:(i-1), ], filename, "wells"),
         throws_error())
   })
   
   test_that(paste("add_plate stops if some wells are missing",
      "leading zeroes and some aren't"), {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = get_well_ids(i), d = letters[1:n_letters])
      complete$d <- as.character(complete$d)
      set.seed(10)
      complete$wells <- ifelse(rnorm(1) < 0, 
         remove_leading_zeroes(complete$wells), 
         complete$wells)
      
      expect_that(add_plate(complete, filename, "wells"),
         throws_error())
   })
   
   test_that("add_plate works with one full plate and one partially empty", {
      filename <- paste0(path, "oneFullOnePartEmpty.csv")
      
      complete <- data.frame(wells = get_well_ids(i), d = letters[1:n_letters],
         stringsAsFactors = FALSE)
      complete$d <- as.character(complete$d)
      
      result <- add_plate(complete, filename, "wells")
      expect_that(result$full, is_identical_to(get_well_ids(i)))
      expect_that(result$full, is_identical_to(result$wells))
      r <- is.na(result$partial) | result$partial == as.character(result$wells)
      expect_that(all(r), is_true())
   })
   
   test_that("add_plate works for complete valid data", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = get_well_ids(i), d = letters[1:n_letters])
      complete$d <- as.character(complete$d)
      
      result <- add_plate(complete, filename, "wells")
      expect_is(result, 
         "tbl_df")
   })
   
   ################################################################################
   context("testing add_plate-wrong_wells_error_message()")
   ################################################################################
   test_that("wrong_wells_error_message returns correct message with 1 well missing", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = get_well_ids(i), d = letters[1:n_letters])
      complete$d <- as.character(complete$d)
      
      annotations <- read_plate(filename, "wellIds")
      annotations <- annotations[!(is.na(annotations$values)), ]
      
      message <- wrong_wells_error_message(complete[1:(i-1), ], "wells", annotations)
      missing <- get_well_ids(i)[i] # get last well
      expected <- paste0("Some wells in your file are not in the data frame ",
         "you provided, but they all should be. The missing wells are: ", 
         missing, ".")
      expect_that(message, is_identical_to(expected))
   })
   
   test_that("wrong_wells_error_message returns correct message with 2 wells missing", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = get_well_ids(i), d = letters[1:n_letters])
      complete$d <- as.character(complete$d)
      
      annotations <- read_plate(filename, "wellIds")
      annotations <- annotations[!(is.na(annotations$values)), ]
      
      message <- wrong_wells_error_message(complete[1:(i-2), ], "wells", annotations)
      missing <- get_well_ids(i)[(i-1):i] # get last two wells
      missing <- paste0(missing, collapse = ", ")
      expected <- paste0("Some wells in your file are not in the data frame ",
         "you provided, but they all should be. The missing wells are: ", 
         missing, ".")
      expect_that(message, is_identical_to(expected))
   })
   
   test_that("wrong_wells_error_message returns correct message with all but 1 well missing", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = get_well_ids(i), d = letters[1:n_letters])
      complete$d <- as.character(complete$d)
      
      annotations <- read_plate(filename, "wellIds")
      annotations <- annotations[!(is.na(annotations$values)), ]
      
      message <- wrong_wells_error_message(complete[1, ], "wells", annotations)
      missing <- get_well_ids(i)[-1] # get all but first
      missing <- paste0(missing, collapse = ", ")
      expected <- paste0("Some wells in your file are not in the data frame ",
         "you provided, but they all should be. The missing wells are: ", 
         missing, ".")
      expect_that(message, is_identical_to(expected))
   })
   
   test_that("wrong_wells_error_message errors with valid input", {
      filename <- paste0(path, "allWellIds.csv")
      complete <- data.frame(wells = get_well_ids(i), d = letters[1:n_letters])
      complete$d <- as.character(complete$d)
      
      annotations <- read_plate(filename, "wellIds")
      annotations <- annotations[!(is.na(annotations$values)), ]
      
      expect_that(wrong_wells_error_message(complete, "wells", annotations), 
         throws_error())
   })
}