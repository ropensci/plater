## ---- echo = FALSE-------------------------------------------------------
# for github flavored markdown, use 
# output:
#   md_document:
#     variant: markdown_github
# then switch back to current output. Just open plateR-basics.md and resave to update time stamp

# print results of code using #>
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
# get the file path to the folder
library(plateR)
system.file("extdata", package = "plateR")

## ------------------------------------------------------------------------
bk <- system.file("extdata", "all-data.csv", package = "plateR")
   
data <- read_plate(plate_size = 96, # total number of wells on the plate
      well_ids_column = "Wells",    # name to give column of well IDs
      file = bk                     # full path to the .csv file
)
str(data)

head(data)

## ------------------------------------------------------------------------
# check concentrations column
view_plate(plate_size = 96, 
      data = data,                         # data frame of interest
      well_ids_column = "Wells",           # name of the column with the well IDs
      column_to_display = "Concentration")   

# check sample column
view_plate(96, data, "Wells", "Sample")

## ------------------------------------------------------------------------
bk2 <- system.file("extdata", "bacterial-killing-one-well-per-row.csv", 
  package = "plateR")

data2 <- read.csv(bk2)

str(data2)

head(data2)

## ------------------------------------------------------------------------
meta <- system.file("extdata", "just-metadata.csv", package = "plateR")
data2 <- add_plate(data = data2,   # data frame to add to    
  plate_size = 96,                 # total number of wells on the plate
      well_ids_column = "Wells",   # name of column of well IDs in data frame
      file = meta                  # full paths to the .csv files
)
str(data2)

head(data2)

