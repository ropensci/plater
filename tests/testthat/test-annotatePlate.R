require(testthat)
path <- "testData/"

################################################################################
context("testing annotatePlate-validatePlate()")
################################################################################

test_that("validate plate fails for incorrect 96-well plate dimensions", {
   bottomRow <- readPlate(paste0(path, "missingBottomRow.csv"))
   expect_that(validatePlate(bottomRow), throws_error())   
   
   rightColumn <- readPlate(paste0(path, "missingRightColumn.csv"))
   expect_that(validatePlate(rightColumn), throws_error())
   
   missingMiddle <- readPlate(paste0(path, "missingMiddleRow.csv"))
   expect_that(validatePlate(missingMiddle), throws_error())   
  
   extraRow <- readPlate(paste0(path, "oneExtraRow.csv"))
   expect_that(validatePlate(extraRow), throws_error())   
   
   extraCol <- readPlate(paste0(path, "oneExtraColumn.csv"))
   expect_that(validatePlate(extraCol), throws_error())
})

test_that("validatePlate() fails for incorrect 96-well row labels", {
   wrongRowLabels <- readPlate(paste0(path, "incorrectRowLabels.csv"))
   expect_that(validatePlate(wrongRowLabels), throws_error()) 
   
   wrongRowLabels <- readPlate(paste0(path, "missingRowLabels.csv"))
   expect_that(validatePlate(wrongRowLabels), throws_error()) 
})

test_that("validatePlate() passes with valid 96-well input", {
   # no error
   plate <- readPlate(paste0(path, "validPlate96Well.csv"))
   validatePlate(plate)
   
   # missing column data, but includes all titles
   plate <- readPlate(paste0(path, "missingColumnsWithCorrectTitles.csv"))
   validatePlate(plate)
})

test_that("validate plate fails for incorrect 384-well plate dimensions", {
   bottomRow <- readPlate(paste0(path, "384missingBottomRow.csv"))
   expect_that(validatePlate(bottomRow), throws_error())   
   
   rightColumn <- readPlate(paste0(path, "384missingRightColumn.csv"))
   expect_that(validatePlate(rightColumn), throws_error())
   
   missingMiddle <- readPlate(paste0(path, "384missingMiddleRow.csv"))
   expect_that(validatePlate(missingMiddle), throws_error())   
   
   extraRow <- readPlate(paste0(path, "384oneExtraRow.csv"))
   expect_that(validatePlate(extraRow), throws_error())   
   
   extraCol <- readPlate(paste0(path, "384oneExtraColumn.csv"))
   expect_that(validatePlate(extraCol), throws_error())
})

test_that("validatePlate() fails for incorrect 384-well row labels", {
   wrongRowLabels <- readPlate(paste0(path, "384incorrectRowLabels.csv"))
   expect_that(validatePlate(wrongRowLabels), throws_error()) 

   wrongRowLabels <- readPlate(paste0(path, "384missingRowLabels.csv"))
   expect_that(validatePlate(wrongRowLabels), throws_error()) 
})

test_that("validatePlate() passes with valid 384-well input", {
   # no error
   plate <- readPlate(paste0(path, "validPlate384Well.csv"))
   validatePlate(plate)
})

################################################################################
context("testing annotatePlate-wrongRowLabelsErrorMessage()")
################################################################################
test_that("wrongRowLabelsErrorMessage() fails for valid plate", {
   validPlate <- readPlate(paste0(path, "validPlate384Well.csv"))
   expect_that(wrongRowLabelsErrorMessage(validPlate), throws_error())
})

test_that("wrongRowLabelsErrorMessage() fails for invalid plate dimensions", {
   missingRow <- readPlate(paste0(path, "missingBottomRow.csv"))
   expect_that(wrongRowLabelsErrorMessage(validPlate), throws_error())
})

test_that("wrongRowLabelsErrorMessage() 96-well", {
   incorrectRowLabels <- readPlate(paste0(path, "incorrectRowLabels.csv"))
   message <- wrongRowLabelsErrorMessage(incorrectRowLabels)
   expect_that(message, matches(
      paste0("Correct row labels not found. Found 'X B C D E F G H' but ",
               "expected 'a b c d e f g h' or 'A B C D E F G H'.")))
})

test_that("wrongRowLabelsErrorMessage() 384-well", {
   incorrectRowLabels384 <- readPlate(paste0(path, "384incorrectRowLabels.csv"))
   message <- wrongRowLabelsErrorMessage(incorrectRowLabels384)
   expect_that(message, matches(
      paste0("Correct row labels not found. Found 'A B C D E F G H N P K L M N",
            " O P' but expected 'a b c d e f g h i j k l m n o p' or 'A B C D",
            " E F G H I J K L M N O P'.")))
})

################################################################################
context("testing annotatePlate-getWellIds()")
################################################################################
test_that("getWellIds() fails for non 96 or 384 input", {
   expect_that(getWellIds(95), throws_error())
})

test_that("getWellIds(96) gives correct output", {
   expected <- c("A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B09", "B10", "B11", "B12", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "D01", "D02", "D03", "D04", "D05", "D06", "D07", "D08", "D09", "D10", "D11", "D12", "E01", "E02", "E03", "E04", "E05", "E06", "E07", "E08", "E09", "E10", "E11", "E12", "F01", "F02", "F03", "F04", "F05", "F06", "F07", "F08", "F09", "F10", "F11", "F12", "G01", "G02", "G03", "G04", "G05", "G06", "G07", "G08", "G09", "G10", "G11", "G12", "H01", "H02", "H03", "H04", "H05", "H06", "H07", "H08", "H09", "H10", "H11", "H12")
   expect_that(getWellIds(96), is_identical_to(expected))
})

test_that("getWellIds(384) gives correct output", {
   expected <- c("A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "A13", "A14", "A15", "A16", "A17", "A18", "A19", "A20", "A21", "A22", "A23", "A24", "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B09", "B10", "B11", "B12", "B13", "B14", "B15", "B16", "B17", "B18", "B19", "B20", "B21", "B22", "B23", "B24", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "D01", "D02", "D03", "D04", "D05", "D06", "D07", "D08", "D09", "D10", "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18", "D19", "D20", "D21", "D22", "D23", "D24", "E01", "E02", "E03", "E04", "E05", "E06", "E07", "E08", "E09", "E10", "E11", "E12", "E13", "E14", "E15", "E16", "E17", "E18", "E19", "E20", "E21", "E22", "E23", "E24", "F01", "F02", "F03", "F04", "F05", "F06", "F07", "F08", "F09", "F10", "F11", "F12", "F13", "F14", "F15", "F16", "F17", "F18", "F19", "F20", "F21", "F22", "F23", "F24", "G01", "G02", "G03", "G04", "G05", "G06", "G07", "G08", "G09", "G10", "G11", "G12", "G13", "G14", "G15", "G16", "G17", "G18", "G19", "G20", "G21", "G22", "G23", "G24", "H01", "H02", "H03", "H04", "H05", "H06", "H07", "H08", "H09", "H10", "H11", "H12", "H13", "H14", "H15", "H16", "H17", "H18", "H19", "H20", "H21", "H22", "H23", "H24", "I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09", "I10", "I11", "I12", "I13", "I14", "I15", "I16", "I17", "I18", "I19", "I20", "I21", "I22", "I23", "I24", "J01", "J02", "J03", "J04", "J05", "J06", "J07", "J08", "J09", "J10", "J11", "J12", "J13", "J14", "J15", "J16", "J17", "J18", "J19", "J20", "J21", "J22", "J23", "J24", "K01", "K02", "K03", "K04", "K05", "K06", "K07", "K08", "K09", "K10", "K11", "K12", "K13", "K14", "K15", "K16", "K17", "K18", "K19", "K20", "K21", "K22", "K23", "K24", "L01", "L02", "L03", "L04", "L05", "L06", "L07", "L08", "L09", "L10", "L11", "L12", "L13", "L14", "L15", "L16", "L17", "L18", "L19", "L20", "L21", "L22", "L23", "L24", "M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12", "M13", "M14", "M15", "M16", "M17", "M18", "M19", "M20", "M21", "M22", "M23", "M24", "N01", "N02", "N03", "N04", "N05", "N06", "N07", "N08", "N09", "N10", "N11", "N12", "N13", "N14", "N15", "N16", "N17", "N18", "N19", "N20", "N21", "N22", "N23", "N24", "O01", "O02", "O03", "O04", "O05", "O06", "O07", "O08", "O09", "O10", "O11", "O12", "O13", "O14", "O15", "O16", "O17", "O18", "O19", "O20", "O21", "O22", "O23", "O24", "P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23", "P24")
   expect_that(getWellIds(384), is_identical_to(expected))
})

################################################################################
context("testing annotatePlate-annotate96WellPlate")
################################################################################
test_that("annotate96WellPlate() gives correct output", {
   # every well present, all have own ID as contents
   plate <- annotate96WellPlate(paste0(path, "allWellIds.csv"), "contents")
   expect_that(plate$contents, is_identical_to(plate$wellIds))
})

test_that("annotate96WellPlate() gives correct output with some empty wells", {
   # every well present, all have own ID as contents
   plate <- annotate96WellPlate(paste0(path, "wellIdsAndEmptyWells.csv"), "contents")
   expect_that(sum(is.na(plate$contents)), equals(27))
   plate <- plate[!is.na(plate$contents), ]
   expect_that(plate$contents, is_identical_to(plate$wellIds))
})

test_that("annotate96WellPlate() gives correct output with blank columnName", {
   # every well present, all have own ID as contents
   plate <- annotate96WellPlate(paste0(path, "allWellIds.csv"))
   expect_that(plate$values, is_identical_to(plate$wellIds))
})