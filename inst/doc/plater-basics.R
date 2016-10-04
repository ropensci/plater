## ---- echo = FALSE-------------------------------------------------------
# for github flavored markdown, use 
# output:
#   md_document:
#     variant: markdown_github
# then switch back to output: rmarkdown::html_vignette. Just open plater-basics.md and resave to update time stamp

library(plater)

# print results of code using #>
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
file_path <- system.file("extdata", "example-1.csv", package = "plater")
   
data <- read_plate(
      file = file_path,             # full path to the .csv file
      well_ids_column = "Wells"     # name to give column of well IDs (optional)
)
str(data)

head(data)

## ------------------------------------------------------------------------
file2A <- system.file("extdata", "example-2-part-A.csv", package = "plater")
data2 <- read.csv(file2A)

str(data2)

head(data2)

meta <- system.file("extdata", "example-2-part-B.csv", package = "plater")
data2 <- add_plate(
      data = data2,               # data frame to add to 
      file = meta,                # full path to the .csv file
      well_ids_column = "Wells"   # name of column of well IDs in data frame
)

str(data2)

head(data2)

## ------------------------------------------------------------------------
# same file as above
file1 <- system.file("extdata", "example-1.csv", package = "plater")

# new file
file2 <- system.file("extdata", "more-bacteria.csv", package = "plater")

data <- read_plates(
   files = c(file1, file2),
   plate_names = c("Experiment 1", "Experiment 2"),
   well_ids_column = "Wells") # optional

str(data)

head(data)

## ------------------------------------------------------------------------
view_plate(
  data = data2, 
  well_ids_column = "Wells", 
  columns_to_display = c("Concentration", "Killing")
)

