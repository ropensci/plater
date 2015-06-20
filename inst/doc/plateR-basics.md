File structure
--------------

When you installed `plateR`, several example .csv files were installed. These files have data and metadata from one experiment and we'll use them to illustrate how `plateR` works. Let's look at them briefly to see how the files should be structured. To find out where the files are stored on your computer, run this code.

``` r
# get the file path to the folder
library(plateR)
system.file("extdata", package = "plateR")
#> [1] "/home/sean/R/x86_64-pc-linux-gnu-library/3.2/plateR/extdata"
```

Now, open up the appropriate folder on your computer. You will see five files:

-   bacterial-killing.csv
-   bacterial-killing-one-well-per-row.csv
-   concentrations.csv
-   samples.csv
-   treatments.csv
-   viability.csv

Open samples.csv in a spreadsheet application.

As you can see, the file is formatted as a microtiter plate. The top-left most cell is empty and the subsequent wells in the top row are labeled 1-12. The subsequent cells in the first column are labeled A-H. This specific example is for a 96-well plate, but for any standard size plate (6-384 wells), the same format applies: empty top left, numbers along the top row to label columns, and letters along the left-column to label rows.

In samples.csv, every well is filled, but that's not necessary. Close that file and open concentrations.csv. Here you see the same structure, but some of the wells are empty. Those wells will simply be represented as `NA` or omitted if none of the other plates has a value there.

### Tips for creating files

It's easy and convenient to make files like this in a spreadsheet program. Just make sure to save them as .csv files. One caution: some spreadsheet programs will include columns in the .csv output if they've ever had text in them, even if they're currently non-empty. So if you get errors about the plate layout being incorrect, try highlighting and deleting 5-10 columns to the right of the plate and rows under the plate and re-saving the file.

Starting from scratch with `read_plate()`
-----------------------------------------

Commonly, an instrument will output data in the form of a plate and that's where you'll want to start. Take this data and copy and paste it into the format described above and save it as a .csv file. Do the same for any additional information you want to include. Each .csv file will be converted into one column in the resulting `data.frame`. Below we illustrate getting the file paths for all the .csv files of interest and then reading them all in at once. Note that we use `system.file()` here to get the file paths of the example files installed with the package, but for your own files you would specify the file path relative to the current working directory without using `system.file()`.

``` r
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
#> 'data.frame':    96 obs. of  6 variables:
#>  $ Wells           : chr  "A01" "A02" "A03" "A04" ...
#>  $ BacterialKilling: int  2 3 2 4 1 2 2 0 5 1 ...
#>  $ Concentration   : num  0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 NA ...
#>  $ Sample          : chr  "Sample A" "Sample B" "Sample C" "Sample A" ...
#>  $ Treatment       : chr  "Drug A" "Drug A" "Drug A" "Drug B" ...
#>  $ Viability       : int  99 99 99 97 99 93 95 98 99 94 ...
head(data)
#>   Wells BacterialKilling Concentration   Sample Treatment Viability
#> 1   A01                2          0.01 Sample A    Drug A        99
#> 2   A02                3          0.01 Sample B    Drug A        99
#> 3   A03                2          0.01 Sample C    Drug A        99
#> 4   A04                4          0.01 Sample A    Drug B        97
#> 5   A05                1          0.01 Sample B    Drug B        99
#> 6   A06                2          0.01 Sample C    Drug B        93
```

To check that the new data frame matched everything up as we expected, we can use `plateR::view_plate()`.

``` r
# check concentrations column
view_plate(plate_size = 96, 
      data = data,                         # data frame of interest
      well_ids_column = "Wells",           # name of the column with the well IDs
      column_to_display = "Concentration")   
#>       1     2     3     4     5     6     7     8     9 10 11 12
#> A  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  0.01  .  .  .
#> B   0.1   0.1   0.1   0.1   0.1   0.1   0.1   0.1   0.1  .  .  .
#> C     1     1     1     1     1     1     1     1     1  .  .  .
#> D    10    10    10    10    10    10    10    10    10  .  .  .
#> E   100   100   100   100   100   100   100   100   100  .  .  .
#> F  1000  1000  1000  1000  1000  1000  1000  1000  1000  .  .  .
#> G 10000 10000 10000 10000 10000 10000 10000 10000 10000  .  .  .
#> H 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05 1e+05  .  .  .

# check sample column
view_plate(96, data, "Wells", "Sample")
#>          1        2        3        4        5        6        7        8
#> A Sample A Sample B Sample C Sample A Sample B Sample C Sample A Sample B
#> B Sample A Sample B Sample C Sample A Sample B Sample C Sample A Sample B
#> C Sample A Sample B Sample C Sample A Sample B Sample C Sample A Sample B
#> D Sample A Sample B Sample C Sample A Sample B Sample C Sample A Sample B
#> E Sample A Sample B Sample C Sample A Sample B Sample C Sample A Sample B
#> F Sample A Sample B Sample C Sample A Sample B Sample C Sample A Sample B
#> G Sample A Sample B Sample C Sample A Sample B Sample C Sample A Sample B
#> H Sample A Sample B Sample C Sample A Sample B Sample C Sample A Sample B
#>          9       10       11       12
#> A Sample C Sample A Sample B Sample C
#> B Sample C Sample A Sample B Sample C
#> C Sample C Sample A Sample B Sample C
#> D Sample C Sample A Sample B Sample C
#> E Sample C Sample A Sample B Sample C
#> F Sample C Sample A Sample B Sample C
#> G Sample C Sample A Sample B Sample C
#> H Sample C Sample A Sample B Sample C
```

Starting with some data with `add_plate()`
==========================================

Sometimes instruments give you data in the form you want: one row per well. But with complicated plate layouts, it can be a pain to match up metadata to the appropriate row. That's where `add_plate()` comes in: you take a data frame that already has one well per row and you add columns from .csv files.

In this case, we'll imagine that the instrument that measured bacterial killing gave a file with one well per row ("bacterial-killing-one-well-per-row.csv"), but that the instrument that measured viability gave us the data in the form of a plate layout as before. All our metadata (concentration, sample, drug) is still in plate format for convenience.

First, read in the data you want to add columns to.

``` r
bk2 <- system.file("extdata", "bacterial-killing-one-well-per-row.csv", 
  package = "plateR")

data2 <- read.csv(bk2)

str(data2)
#> 'data.frame':    96 obs. of  2 variables:
#>  $ Wells           : Factor w/ 96 levels "A01","A02","A03",..: 1 2 3 4 5 6 7 8 9 10 ...
#>  $ BacterialKilling: int  2 3 2 4 1 2 2 0 5 1 ...
head(data2)
#>   Wells BacterialKilling
#> 1   A01                2
#> 2   A02                3
#> 3   A03                2
#> 4   A04                4
#> 5   A05                1
#> 6   A06                2
```

Now, we want to add the data from the plate layout files to this data frame and match it up by wells. Note that it doesn't matter what order the wells are in in `data2`. In the call to `add_plate` we'll specify which column in `data2` has the well IDs and it'll match it up that way.

``` r
data2 <- add_plate(data = data2,    # data frame to add to    
  plate_size = 96,                 # total number of wells on the plate
      well_ids_column = "Wells",   # name of column of well IDs in data frame
      file_names =                 # full paths to the .csv files
         c(concentrations, samples, treatments, viability), 
      column_names =               # names to give each new column
         c("Concentration", "Sample", "Treatment", "Viability"))
str(data2)
#> 'data.frame':    96 obs. of  6 variables:
#>  $ Wells           : Factor w/ 96 levels "A01","A02","A03",..: 1 2 3 4 5 6 7 8 9 10 ...
#>  $ BacterialKilling: int  2 3 2 4 1 2 2 0 5 1 ...
#>  $ Concentration   : num  0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 NA ...
#>  $ Sample          : chr  "Sample A" "Sample B" "Sample C" "Sample A" ...
#>  $ Treatment       : chr  "Drug A" "Drug A" "Drug A" "Drug B" ...
#>  $ Viability       : int  99 99 99 97 99 93 95 98 99 94 ...
head(data2)
#>   Wells BacterialKilling Concentration   Sample Treatment Viability
#> 1   A01                2          0.01 Sample A    Drug A        99
#> 2   A02                3          0.01 Sample B    Drug A        99
#> 3   A03                2          0.01 Sample C    Drug A        99
#> 4   A04                4          0.01 Sample A    Drug B        97
#> 5   A05                1          0.01 Sample B    Drug B        99
#> 6   A06                2          0.01 Sample C    Drug B        93
```
