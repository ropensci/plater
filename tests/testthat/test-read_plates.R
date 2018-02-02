################################################################################
context("testing read_plates-generate_plate_names")
################################################################################
sample_dirs <- c(
   "x.csv",
   "~/A/b/C/d/x.csv", 
   "~/A/b/C/d/x.CSV", 
   "~/A/b/C/d/x.Csv", 
   "\\A\\b\\C\\d\\x.csv",
   "\\A\\b\\C\\d\\x.CSV",
   "\\A\\b\\C\\d\\x.Csv",
   "\\my folder with white space\\b\\C\\d\\a long file name with numbers 23 and symbols *%&.Csv"
)

test_that("generate_plate_names should work with .csv of any case, arbitrary file paths before the file name, whitespace",
   expect_that(generate_plate_names(sample_dirs), 
      is_identical_to(
         c(rep("x", 7), "a long file name with numbers 23 and symbols *%&"))))

################################################################################
context("testing read_plates-read_plates")
################################################################################
for (i in c(6, 12, 24, 48, 96, 384, 1536)) {
   path <- paste0("testData/", i, "/")
   
   plates <- c("read_plates-validOne", "read_plates-validTwo")
   example_plate <- read_plates(paste0(path, plates, ".csv")) 
   
   test_that("read_plates should contain all columns present in *any* of the input data frames", {
      expect_that(colnames(example_plate), 
         is_identical_to(c("Plate", "Wells", "Donor", "Drug", "Media")))
   })

   test_that("Plate should be first column in result of read_plates", {
      expect_that(colnames(example_plate)[1], 
         is_identical_to("Plate"))
   })
      
   test_that("read_plates without plate_names arg should be named by files", {
      expect_that(unique(example_plate$Plate), is_identical_to(plates))
   })
   
   test_that("read_plates with plate_names arg should be correctly named", {
      p <- read_plates(paste0(path, plates, ".csv"), c("A", "B"))
      expect_that(unique(p$Plate), is_identical_to(c("A", "B")))
   })
   
   test_that("read_plates with unequal length files and plate_names should error", {
      expect_error(read_plates(paste0(path, plates, ".csv"), 1:3), 
         "same length")
   })
   
   test_that("read_plates returns a tbl_df", {
      expect_is(example_plate, "tbl_df")
   })
   
   test_that("read_plates should report name of plate causing error", {
      expect_error(read_plates(
         paste0(path, c("read_plates-validOne.csv", "incorrectRowLabels.csv"))), 
      "in file 'incorrectRowLabels'")
   })
   
   test_that("read_plates works with a plate containing only one well", {
      result <- read_plates(paste0(path, c(plates[1], "oneWell"), ".csv"))
      expect_true(all(is.na(result$values) | result$values == "singleton"))
   })
}