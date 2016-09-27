for (i in c(12, 24, 48, 96, 384)) {
   path <- paste0("testData/", i, "/")
   
   ################################################################################
   context("testing convert_plate_to_column-validate_plate()")
   ################################################################################
   get_file_for_validate_plate <- function(file) {
      result <- readLines(paste0(path, file))
      
      # remove top row
      result[2:(length(result))]
   }
   
   test_that("validate plate fails for incorrect plate dimensions", {
      bottomRow <- plate_text_to_data_frame(get_file_for_validate_plate("missingBottomRow.csv"))
      expect_that(validate_plate(bottomRow), throws_error())   
      
      rightColumn <- plate_text_to_data_frame(get_file_for_validate_plate("missingRightColumn.csv"))
      expect_that(validate_plate(rightColumn), throws_error())
      
      missingMiddle <- plate_text_to_data_frame(get_file_for_validate_plate("missingMiddleRow.csv"))
      expect_that(validate_plate(missingMiddle), throws_error())   
     
      extraRow <- plate_text_to_data_frame(get_file_for_validate_plate("oneExtraRow.csv"))
      expect_that(validate_plate(extraRow), throws_error())   
      
      extraCol <- plate_text_to_data_frame(get_file_for_validate_plate("oneExtraColumn.csv"))
      expect_that(validate_plate(extraCol), throws_error())
   })
   
   test_that("validate_plate() fails for incorrect row labels", {
      wrongRowLabels <- plate_text_to_data_frame(get_file_for_validate_plate("incorrectRowLabels.csv"))
      expect_that(validate_plate(wrongRowLabels), throws_error()) 
      
      wrongRowLabels <- plate_text_to_data_frame(get_file_for_validate_plate("missingRowLabels.csv"))
      expect_that(validate_plate(wrongRowLabels), throws_error()) 
   })
   
   test_that("validate_plate() passes with valid input", {
      # no error
      plate <- plate_text_to_data_frame(get_file_for_validate_plate("validPlate.csv"))
      validate_plate(plate, i)
      
      # missing column data, but includes all titles
      plate <- plate_text_to_data_frame(get_file_for_validate_plate("missingColumnsWithCorrectTitles.csv"))
      validate_plate(plate, i)
   })
   
   test_that("validate_plate() passes with white space around row labels", {
      # no error
      # contains spaces in the row labels
      plate <- plate_text_to_data_frame(get_file_for_validate_plate("validPlateWithWhiteSpaceInRowNames.csv"))
      validate_plate(plate, i)
   })
   
   ################################################################################
   context("testing convert_plate_to_column-wrong_row_labels_error_message()")
   ################################################################################
   
   test_that("wrong_row_labels_error_message() fails for invalid plate dimensions", {
      missingRow <- plate_text_to_data_frame(get_file_for_validate_plate("missingBottomRow.csv"))
      expect_that(wrong_row_labels_error_message(validPlate, i), throws_error())
   })
   
   test_that("wrong_row_labels_error_message()", {
      incorrectRowLabels <- plate_text_to_data_frame(get_file_for_validate_plate("incorrectRowLabels.csv"))
      message <- wrong_row_labels_error_message(incorrectRowLabels, i)
      
      rows <- number_of_rows(i)
      wrong <- paste(c("X", LETTERS[2:rows]), collapse = " ")
      lower <- paste(letters[1:rows], collapse = " ")
      upper <- paste(LETTERS[1:rows], collapse = " ")
      
      expect_that(message, matches(
         paste0("Correct row labels not found. Found '", wrong, "' but ",
                  "expected '", lower, "' or '", upper,"'.")))
   })
   
   ################################################################################
   context("testing convert_plate_to_column-convert_plate_to_column")
   ################################################################################
   getFileForConvertPlate <- function(file) {
      readLines(paste0(path, file))
   }
   
   test_that("convert_plate_to_column() gives correct output", {
      # every well present, all have own ID as contents
      plate <- convert_plate_to_column(getFileForConvertPlate("allWellIds.csv"), i)   
      expect_that(plate$values, is_identical_to(plate$wellIds))
   })
   
   # as.matrix converts numeric columns to character via format() which adds 
   # white space to right-align the digits, so when some columns are numeric and
   # some columns are character AND there are different numbers of digits (or 
   # digits and number of chars in NA), some numbers will have white space added
   # which is undesireable. as.matrix() is called by t(), which is how 
   # convert_plate_to_column() used to work. This was fixed in commit 
   # 0bc010b66ef98281a47caf94a39b70417969da3f by no longer using a matrix 
   # intermediate. This test is to ensure that this bug is not accidentally 
   # reintroduced
   test_that("convert_plate_to_column() doesn't add unwanted white space", {
      # every well present, all have own ID as contents
      plate <- convert_plate_to_column(getFileForConvertPlate("dontAddWhiteSpace.csv"), i)   
      expect_that(plate$values, is_identical_to(c("1", "A", "20", "B")))
   })
}
