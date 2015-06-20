## ---- echo = FALSE-------------------------------------------------------
# for github flavored markdown, use 
# output:
#   md_document:
#     variant: markdown_github

# print results of code using #>
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
# get the file path to the folder
library(plateR)
system.file("extdata", package = "plateR")

## ------------------------------------------------------------------------
bk <- system.file("extdata", "bacterial-killing.csv", package = "plateR")
concentrations <- system.file("extdata", "concentrations.csv", package = "plateR")
samples <- system.file("extdata", "samples.csv", package = "plateR")
treatments <- system.file("extdata", "treatments.csv", package = "plateR")
viability <- system.file("extdata", "viability.csv", package = "plateR")
   
data <- read_plate(plate_size = 96, # total number of wells on the plate
      well_ids_column = "Wells",    # name to give column of well IDs
      file_names =                  # full paths to the .csv files
         c(bk, concentrations, samples, treatments, viability), 
      column_names =                # names to give each new column
         c("BacterialKilling", "Concentration", "Sample", "Treatment", 
            "Viability"))
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
data2 <- add_plate(data = data2,    # data frame to add to    
  plate_size = 96,                 # total number of wells on the plate
      well_ids_column = "Wells",   # name of column of well IDs in data frame
      file_names =                 # full paths to the .csv files
         c(concentrations, samples, treatments, viability), 
      column_names =               # names to give each new column
         c("Concentration", "Sample", "Treatment", "Viability"))
str(data2)
head(data2)

