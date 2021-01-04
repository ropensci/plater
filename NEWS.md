# plater 1.0.3
* Change `add_plate()` to return a tibble rather than trying to preserve initial class
* Remove use of deprecated `select_` function

# plater 1.0.2
* Changes to tests to comply with new CRAN policy on `data.frame(..., stringsAsFactors = FALSE)`
* Add support for 6- and 1536-well plates
* Change behavior of add_plate so that when the plate layout contains more wells than the input data frame, those wells are appended to the end of the data frame instead of erroring. 

# plater 1.0.1
* Eliminate warnings from readLines on files without EOF (Mac issue)
* Fix issue with numeric formatting in mixed numeric/character layouts
* Fix issue with grouped tibbles and view_plate

# plater 1.0.0 (5 Oct 2016)
* Changes in response to rOpenSci reviewers
* Reorder arguments of `add_plate()` for better pipelining
* add `check_plater_format()` to help with preparing files
* rename all lowercase

# plateR 0.2.1
* Reorganize parameters for consistency
* Add defaults for parameters
* Add `read_plates()`

# plateR 0.2
* Introduce new data format with multiple plate layouts per .csv file (replacing multiple files at once)

# plateR 0.1
* Add support for reading multiple files at once