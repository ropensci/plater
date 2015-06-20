## ---- echo = FALSE-------------------------------------------------------
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

## ------------------------------------------------------------------------
view_plate(plate_size = 96, 
      data = data,                         # data frame of interest
      well_ids_column = "Wells",           # name of the column with the well IDs
      column_to_display = "Concentration")   

view_plate(96, data, "Wells", "Treatment")

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

