## ---- echo = FALSE-------------------------------------------------------
# for github flavored markdown, use 
# output:
#   md_document:
#     variant: markdown_github
# then switch back to output: rmarkdown::html_vignette. Just open plateR-basics.md and resave to update time stamp

# print results of code using #>
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ---- echo = FALSE-------------------------------------------------------
library(plateR)
bk <- system.file("extdata", "example-1.csv", package = "plateR")
   
data <- read_plate(bk)

view_plate(data, "Wells", "BacterialKilling")

## ---- echo=FALSE---------------------------------------------------------
data$Treatment <- gsub("Drug ", "", data$Treatment)
data$Treatment <- gsub("Control", "Ctrl", data$Treatment)

view_plate(data, "Wells", "Treatment")

## ---- echo=FALSE---------------------------------------------------------
head(data[, c("Wells", "BacterialKilling", "Treatment")])

## ------------------------------------------------------------------------
# get the file path to the folder
library(plateR)
system.file("extdata", package = "plateR")

## ------------------------------------------------------------------------
bk <- system.file("extdata", "example-1.csv", package = "plateR")
   
data <- read_plate(
      file = bk,                    # full path to the .csv file
      well_ids_column = "Wells",    # name to give column of well IDs (optional)
      plate_size = 96               # total number of wells on the plate (optional)
)
str(data)

head(data)

## ------------------------------------------------------------------------
bk2 <- system.file("extdata", "example-2-part-A.csv", 
  package = "plateR")

data2 <- read.csv(bk2)

str(data2)

head(data2)

## ------------------------------------------------------------------------
meta <- system.file("extdata", "example-2-part-B.csv", package = "plateR")
data2 <- add_plate(
      file = meta,                # full path to the .csv file
      data = data2,               # data frame to add to    
      well_ids_column = "Wells",  # name of column of well IDs in data frame
      plate_size = 96             # total number of wells on the plate (optional)
)
str(data2)

head(data2)

## ------------------------------------------------------------------------
view_plate( 
      data = data2,                         # data frame of interest
      well_ids_column = "Wells",           # name of the column with the well IDs
      column_to_display = "Concentration", # column to display 
      plate_size = 96                      # total number of wells on the plate (optional)
)   

