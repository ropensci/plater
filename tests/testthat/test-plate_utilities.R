################################################################################
context("testing plateUtilities-get_well_ids()")
################################################################################
test_that("get_well_ids() fails for non-supported plateSize", {
   expect_that(get_well_ids(95), throws_error())
})

test_that("get_well_ids(12) gives correct output", {
   expected <- c("A01", "A02", "A03", "A04", "B01", "B02", "B03", "B04", "C01", "C02", "C03", "C04")
   expect_that(get_well_ids(12), is_identical_to(expected))
})

test_that("get_well_ids(24) gives correct output", {
   expected <- c("A01", "A02", "A03", "A04", "A05", "A06", "B01", "B02", "B03", "B04", "B05", "B06", "C01", "C02", "C03", "C04", "C05", "C06", "D01", "D02", "D03", "D04", "D05", "D06")
   expect_that(get_well_ids(24), is_identical_to(expected))
})

test_that("get_well_ids(48) gives correct output", {
   expected <- c("A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "D01", "D02", "D03", "D04", "D05", "D06", "D07", "D08", "E01", "E02", "E03", "E04", "E05", "E06", "E07", "E08", "F01", "F02", "F03", "F04", "F05", "F06", "F07", "F08")
   expect_that(get_well_ids(48), is_identical_to(expected))
})

test_that("get_well_ids(96) gives correct output", {
   expected <- c("A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B09", "B10", "B11", "B12", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "D01", "D02", "D03", "D04", "D05", "D06", "D07", "D08", "D09", "D10", "D11", "D12", "E01", "E02", "E03", "E04", "E05", "E06", "E07", "E08", "E09", "E10", "E11", "E12", "F01", "F02", "F03", "F04", "F05", "F06", "F07", "F08", "F09", "F10", "F11", "F12", "G01", "G02", "G03", "G04", "G05", "G06", "G07", "G08", "G09", "G10", "G11", "G12", "H01", "H02", "H03", "H04", "H05", "H06", "H07", "H08", "H09", "H10", "H11", "H12")
   expect_that(get_well_ids(96), is_identical_to(expected))
})

test_that("get_well_ids(384) gives correct output", {
   expected <- c("A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12", "A13", "A14", "A15", "A16", "A17", "A18", "A19", "A20", "A21", "A22", "A23", "A24", "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B09", "B10", "B11", "B12", "B13", "B14", "B15", "B16", "B17", "B18", "B19", "B20", "B21", "B22", "B23", "B24", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "D01", "D02", "D03", "D04", "D05", "D06", "D07", "D08", "D09", "D10", "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18", "D19", "D20", "D21", "D22", "D23", "D24", "E01", "E02", "E03", "E04", "E05", "E06", "E07", "E08", "E09", "E10", "E11", "E12", "E13", "E14", "E15", "E16", "E17", "E18", "E19", "E20", "E21", "E22", "E23", "E24", "F01", "F02", "F03", "F04", "F05", "F06", "F07", "F08", "F09", "F10", "F11", "F12", "F13", "F14", "F15", "F16", "F17", "F18", "F19", "F20", "F21", "F22", "F23", "F24", "G01", "G02", "G03", "G04", "G05", "G06", "G07", "G08", "G09", "G10", "G11", "G12", "G13", "G14", "G15", "G16", "G17", "G18", "G19", "G20", "G21", "G22", "G23", "G24", "H01", "H02", "H03", "H04", "H05", "H06", "H07", "H08", "H09", "H10", "H11", "H12", "H13", "H14", "H15", "H16", "H17", "H18", "H19", "H20", "H21", "H22", "H23", "H24", "I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09", "I10", "I11", "I12", "I13", "I14", "I15", "I16", "I17", "I18", "I19", "I20", "I21", "I22", "I23", "I24", "J01", "J02", "J03", "J04", "J05", "J06", "J07", "J08", "J09", "J10", "J11", "J12", "J13", "J14", "J15", "J16", "J17", "J18", "J19", "J20", "J21", "J22", "J23", "J24", "K01", "K02", "K03", "K04", "K05", "K06", "K07", "K08", "K09", "K10", "K11", "K12", "K13", "K14", "K15", "K16", "K17", "K18", "K19", "K20", "K21", "K22", "K23", "K24", "L01", "L02", "L03", "L04", "L05", "L06", "L07", "L08", "L09", "L10", "L11", "L12", "L13", "L14", "L15", "L16", "L17", "L18", "L19", "L20", "L21", "L22", "L23", "L24", "M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12", "M13", "M14", "M15", "M16", "M17", "M18", "M19", "M20", "M21", "M22", "M23", "M24", "N01", "N02", "N03", "N04", "N05", "N06", "N07", "N08", "N09", "N10", "N11", "N12", "N13", "N14", "N15", "N16", "N17", "N18", "N19", "N20", "N21", "N22", "N23", "N24", "O01", "O02", "O03", "O04", "O05", "O06", "O07", "O08", "O09", "O10", "O11", "O12", "O13", "O14", "O15", "O16", "O17", "O18", "O19", "O20", "O21", "O22", "O23", "O24", "P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23", "P24")
   expect_that(get_well_ids(384), is_identical_to(expected))
})

################################################################################
context("testing plateUtilities-get_well_ids_without_leading_zeroes()")
################################################################################
test_that("get_well_ids_without_leading_zeroes() fails for non-supported plateSize", {
   expect_that(get_well_ids_without_leading_zeroes(95), throws_error())
})

test_that("get_well_ids_without_leading_zeroes(12) gives correct output", {
   expected <- c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4", "C1", "C2", "C3", "C4")
   expect_that(get_well_ids_without_leading_zeroes(12), is_identical_to(expected))
})

test_that("get_well_ids_without_leading_zeroes(24) gives correct output", {
   expected <- c("A1", "A2", "A3", "A4", "A5", "A6", "B1", "B2", "B3", "B4", "B5", "B6", "C1", "C2", "C3", "C4", "C5", "C6", "D1", "D2", "D3", "D4", "D5", "D6")
   expect_that(get_well_ids_without_leading_zeroes(24), is_identical_to(expected))
})

test_that("get_well_ids_without_leading_zeroes(48) gives correct output", {
   expected <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8")
   expect_that(get_well_ids_without_leading_zeroes(48), is_identical_to(expected))
})

test_that("get_well_ids_without_leading_zeroes(96) gives correct output", {
   expected <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12", "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12", "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11", "H12")
   expect_that(get_well_ids_without_leading_zeroes(96), is_identical_to(expected))
})

test_that("get_well_ids(384) gives correct output", {
   expected <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12", "A13", "A14", "A15", "A16", "A17", "A18", "A19", "A20", "A21", "A22", "A23", "A24", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12", "B13", "B14", "B15", "B16", "B17", "B18", "B19", "B20", "B21", "B22", "B23", "B24", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18", "D19", "D20", "D21", "D22", "D23", "D24", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12", "E13", "E14", "E15", "E16", "E17", "E18", "E19", "E20", "E21", "E22", "E23", "E24", "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12", "F13", "F14", "F15", "F16", "F17", "F18", "F19", "F20", "F21", "F22", "F23", "F24", "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12", "G13", "G14", "G15", "G16", "G17", "G18", "G19", "G20", "G21", "G22", "G23", "G24", "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11", "H12", "H13", "H14", "H15", "H16", "H17", "H18", "H19", "H20", "H21", "H22", "H23", "H24", "I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12", "I13", "I14", "I15", "I16", "I17", "I18", "I19", "I20", "I21", "I22", "I23", "I24", "J1", "J2", "J3", "J4", "J5", "J6", "J7", "J8", "J9", "J10", "J11", "J12", "J13", "J14", "J15", "J16", "J17", "J18", "J19", "J20", "J21", "J22", "J23", "J24", "K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9", "K10", "K11", "K12", "K13", "K14", "K15", "K16", "K17", "K18", "K19", "K20", "K21", "K22", "K23", "K24", "L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9", "L10", "L11", "L12", "L13", "L14", "L15", "L16", "L17", "L18", "L19", "L20", "L21", "L22", "L23", "L24", "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11", "M12", "M13", "M14", "M15", "M16", "M17", "M18", "M19", "M20", "M21", "M22", "M23", "M24", "N1", "N2", "N3", "N4", "N5", "N6", "N7", "N8", "N9", "N10", "N11", "N12", "N13", "N14", "N15", "N16", "N17", "N18", "N19", "N20", "N21", "N22", "N23", "N24", "O1", "O2", "O3", "O4", "O5", "O6", "O7", "O8", "O9", "O10", "O11", "O12", "O13", "O14", "O15", "O16", "O17", "O18", "O19", "O20", "O21", "O22", "O23", "O24", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23", "P24")
   expect_that(get_well_ids_without_leading_zeroes(384), is_identical_to(expected))
})

################################################################################
context("testing plateUtilities-remove_leading_zeroes()")
################################################################################
test_that("test remove_leading_zeroes() single well ID", {
   expect_that(remove_leading_zeroes("A01"), is_identical_to("A1"))
})

test_that("test remove_leading_zeroes() vector of well IDs", {
   expect_that(remove_leading_zeroes(c("A01", "A02", "A01")), 
      is_identical_to(c("A1", "A2", "A1")))
})

test_that("test remove_leading_zeroes() single well ID no change", {
   expect_that(remove_leading_zeroes("A1"), is_identical_to("A1"))
})

test_that("test remove_leading_zeroes() vector of well IDs no change", {
   expect_that(remove_leading_zeroes(c("A1", "A2", "A1")), 
      is_identical_to(c("A1", "A2", "A1")))
})

test_that("test remove_leading_zeroes() vector of well IDs some change some no change", {
   expect_that(remove_leading_zeroes(c("A01", "A02", "A1")), 
      is_identical_to(c("A1", "A2", "A1")))
})

################################################################################
context("testing plateUtilities-number_of_rows()")
################################################################################
test_that("test number_of_rows(12)", {
   expect_that(number_of_rows(12), equals(3))
})

test_that("test number_of_rows(24)", {
   expect_that(number_of_rows(24), equals(4))
})

test_that("test number_of_rows(48)", {
   expect_that(number_of_rows(48), equals(6))
})

test_that("test number_of_rows(96)", {
   expect_that(number_of_rows(96), equals(8))
})

test_that("test number_of_rows(384)", {
   expect_that(number_of_rows(384), equals(16))
})

test_that("test number_of_rows() non-96 or 384 fails", {
   for(i in c(-1, 0, 1, 95, 97)) {
      expect_that(number_of_rows(i), throws_error())
   }
})

################################################################################
context("testing plateUtilities-number_of_columns()")
################################################################################
test_that("test number_of_columns(12)", {
   expect_that(number_of_columns(12), equals(4))
})

test_that("test number_of_columns(24)", {
   expect_that(number_of_columns(24), equals(6))
})

test_that("test number_of_columns(48)", {
   expect_that(number_of_columns(48), equals(8))
})

test_that("test number_of_columns(96)", {
   expect_that(number_of_columns(96), equals(12))
})

test_that("test number_of_columns(384)", {
   expect_that(number_of_columns(384), equals(24))
})

test_that("test number_of_columns() non-supported size fails", {
   for(i in c(-1, 0, 1, 95, 97)) {
      expect_that(number_of_columns(i), throws_error())
   }
})

################################################################################
context("testing plateUtilities-validate_well_idsColumn()")
################################################################################
test_that("validate_well_idsColumn() throws error with missing column", {
   expect_that(validate_well_ids(data.frame(Wells = 1:10), "Wezlls"), 
      throws_error())
})

################################################################################
context("testing plateUtilities-plate_dimensions()")
################################################################################
test_that("test columns", {
   expect_that(plate_dimensions("Columns", "PlateSize", 12), equals(4))
   expect_that(plate_dimensions("Columns", "PlateSize", 24), equals(6))
   expect_that(plate_dimensions("Columns", "PlateSize", 48), equals(8))
   expect_that(plate_dimensions("Columns", "PlateSize", 96), equals(12))
   expect_that(plate_dimensions("Columns", "PlateSize", 384), equals(24))
})

test_that("test rows", {
   expect_that(plate_dimensions("Rows", "PlateSize", 12), equals(3))
   expect_that(plate_dimensions("Rows", "PlateSize", 24), equals(4))
   expect_that(plate_dimensions("Rows", "PlateSize", 48), equals(6))
   expect_that(plate_dimensions("Rows", "PlateSize", 96), equals(8))
   expect_that(plate_dimensions("Rows", "PlateSize", 384), equals(16))
})

test_that("test plate size", {
   expect_that(plate_dimensions("PlateSize", "Columns", 4), equals(12))
   expect_that(plate_dimensions("PlateSize", "Columns", 6), equals(24))
   expect_that(plate_dimensions("PlateSize", "Columns", 8), equals(48))
   expect_that(plate_dimensions("PlateSize", "Columns", 12), equals(96))
   expect_that(plate_dimensions("PlateSize", "Columns", 24), equals(384))
})

test_that("test invalids", {
   expect_that(length(plate_dimensions("PlateSize", "Columns", 1)), equals(0))
   expect_that(length(plate_dimensions("Columns", "PlateSize", -1)), equals(0))
   expect_that(length(plate_dimensions("PlateSize", "Rows", 9)), equals(0))
})

################################################################################
context("testing plateUtilities-get_plate_size_from_number_of_columns()")
################################################################################
test_that("valid input", {
   expect_that(get_plate_size_from_number_of_columns(4), equals(12))
   expect_that(get_plate_size_from_number_of_columns(6), equals(24))
   expect_that(get_plate_size_from_number_of_columns(8), equals(48))
   expect_that(get_plate_size_from_number_of_columns(12), equals(96))
   expect_that(get_plate_size_from_number_of_columns(24), equals(384))
})

test_that("invalid column size", 
   expect_error(get_plate_size_from_number_of_columns(17),
      paste0("Could not guess plate size from number of columns. ", 
         "Invalid number of columns: ", 17)))

################################################################################
context("testing plate_utilities-validate_column_is_in_data()")
################################################################################
d <- data.frame(ColumnA = 1, ColumnB = 1, ColumnC = 1)

test_that("valid one column input", 
   expect_silent(validate_column_is_in_data(d, "ColumnA")))

test_that("valid two column input", 
   expect_silent(validate_column_is_in_data(d, c("ColumnA", "ColumnB"))))

test_that("valid two column input out of order", 
   expect_silent(validate_column_is_in_data(d, c("ColumnB", "ColumnA"))))

test_that("invalid single column", 
   expect_error(validate_column_is_in_data(d, "ColumnD"), 
      "There is no column named 'ColumnD' in your data frame."))

test_that("invalid double column", 
   expect_error(validate_column_is_in_data(d, c("ColumnD", "ColumnE")), 
      "There are no columns named ColumnD, ColumnE in your data frame."))

################################################################################
context("testing plate_utilities-check_file_path()")
################################################################################
# check_file_path
test_that("invalid file path", 
  expect_error(check_file_path("fake_path.csv"), 
    "Sorry, can't find your file 'fake_path.csv'."))

test_that("NULL file path", 
  expect_error(check_file_path(NULL), 
    "Sorry, can't find your file ''."))

test_that("bad file extension", 
  expect_error(check_file_path("testData/file-path-tests/bad_extension"),
    "Sorry, 'testData/file-path-tests/bad_extension' doesn't have a proper CSV file extension."))

test_that("valid file", 
  expect_silent(check_file_path("testData/96/allWellIds.csv")))

# these valid file paths are not valid CSV files so should only be checked with
# check_file_path, not with read_plate, etc, because they'll fail later checks
test_that("valid files with various extensions", 
  expect_silent(c(
    check_file_path("testData/file-path-tests/good_extension.CSV"),
    check_file_path("testData/file-path-tests/good_extension.CsV"),
    check_file_path("testData/file-path-tests/good_extension.csv"))))

# check_file_path in context of read_plate
test_that("invalid file path with read_plate", 
  expect_error(read_plate("fake_path.csv"), 
    "Sorry, can't find your file 'fake_path.csv'."))

test_that("NULL file path with read_plate", 
  expect_error(read_plate(NULL), 
    "Sorry, can't find your file ''."))

test_that("bad file extension with read_plate", 
  expect_error(read_plate("testData/file-path-tests/bad_extension"),
    "Sorry, 'testData/file-path-tests/bad_extension' doesn't have a proper CSV file extension."))

test_that("valid file with read_plate", 
  expect_silent(read_plate("testData/96/allWellIds.csv")))

# check_file_path in context of read_plates
test_that("invalid file path with read_plates", 
  expect_error(read_plates(c("fake_path.csv", "testData/96/allWellIds.csv")), 
    "Sorry, can't find your file 'fake_path.csv'."))

test_that("invalid file path with read_plates reverse order", 
  expect_error(read_plates(c("testData/96/allWellIds.csv", "fake_path.csv")), 
    "Sorry, can't find your file 'fake_path.csv'."))

test_that("bad file extension with read_plates", 
  expect_error(
    read_plates(c("testData/file-path-tests/bad_extension", "testData/96/allWellIds.csv")),
    "Sorry, 'testData/file-path-tests/bad_extension' doesn't have a proper CSV file extension."))

test_that("bad file extension with read_plates reverse order", 
  expect_error(
    read_plates(c("testData/96/allWellIds.csv", "testData/file-path-tests/bad_extension")),
    "Sorry, 'testData/file-path-tests/bad_extension' doesn't have a proper CSV file extension."))

test_that("valid file with read_plates", 
  expect_silent(read_plates(c("testData/96/allWellIds.csv", "testData/96/allWellIds.csv"))))

# check_file_path in context of add_plate
add_plate_dummy_df <- data.frame(wells = get_well_ids(96))

test_that("invalid file path with add_plate", 
  expect_error(add_plate(add_plate_dummy_df, "fake_path.csv"), 
    "Sorry, can't find your file 'fake_path.csv'."))

test_that("NULL file path with add_plate", 
  expect_error(add_plate(add_plate_dummy_df, NULL), 
    "Sorry, can't find your file ''."))

test_that("bad file extension with add_plate", 
  expect_error(add_plate(add_plate_dummy_df, "testData/file-path-tests/bad_extension"),
    "Sorry, 'testData/file-path-tests/bad_extension' doesn't have a proper CSV file extension."))

test_that("valid file with add_plate", 
  expect_silent(add_plate(add_plate_dummy_df, "testData/96/allWellIds.csv", "wells")))

################################################################################
context("testing plate_utilities-check_well_ids_column_name()")
################################################################################
test_that("invalid well ids column name", 
  expect_error(check_well_ids_column_name(""), 
    "Sorry, well_ids_column must not be NULL or an empty string."))

test_that("invalid well ids column name", 
  expect_error(check_well_ids_column_name(NULL), 
    "Sorry, well_ids_column must not be NULL or an empty string."))

test_that("invalid well ids column name", 
  expect_error(read_plate("testData/96/allWellIds.csv", well_ids_column = NULL), 
    "Sorry, well_ids_column must not be NULL or an empty string."))

test_that("invalid well ids column name", 
  expect_error(read_plate("testData/96/allWellIds.csv", well_ids_column = ""), 
    "Sorry, well_ids_column must not be NULL or an empty string."))

test_that("invalid well ids column name", 
  expect_error(add_plate(data.frame(xyz = 1), 
    "testData/96/allWellIds.csv", well_ids_column = ""), 
    "Sorry, well_ids_column must not be NULL or an empty string."))

test_that("invalid well ids column name", 
  expect_error(add_plate(data.frame(xyz = 1), "testData/96/allWellIds.csv", 
    well_ids_column = NULL), 
    "Sorry, well_ids_column must not be NULL or an empty string."))

test_that("invalid well ids column name", 
  expect_error(read_plates("testData/96/allWellIds.csv", well_ids_column = ""), 
    "Sorry, well_ids_column must not be NULL or an empty string."))

test_that("invalid well ids column name", 
  expect_error(read_plates("testData/96/allWellIds.csv", well_ids_column = NULL), 
    "Sorry, well_ids_column must not be NULL or an empty string."))

test_that("invalid well ids column name", 
  expect_error(view_plate(data.frame(test = 1), "", "zing"), 
    "Sorry, well_ids_column must not be NULL or an empty string."))

test_that("invalid well ids column name", 
  expect_error(view_plate(data.frame(test = 1), NULL, "zing"), 
    "Sorry, well_ids_column must not be NULL or an empty string."))