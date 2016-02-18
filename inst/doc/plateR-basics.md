File structure
--------------

When you installed `plateR`, several example .csv files were installed. These files have data and metadata from one experiment and we'll use them to illustrate how `plateR` works. Let's look at them briefly to see how the files should be structured. To find out where the files are stored on your computer, run this code.

``` r
# get the file path to the folder
library(plateR)
system.file("extdata", package = "plateR")
#> [1] "C:/Users/smhughes/Documents/R/win-library/3.2/plateR/extdata"
```

Now, open up the appropriate folder on your computer. You will see three files:

-   example-1.csv
-   example-2-data.csv
-   example-2-metadata.csv

Open example-1.csv in a spreadsheet application.

As you can see, the file is formatted as several microtiter plates. The top-left most cell has the name of the information in that plate and the subsequent wells in the top row are labeled 1-12. The subsequent cells in the first column are labeled A-H. Plates are separated by an empty row. This specific example is for a 96-well plate, but for any standard size plate (12-384 wells), the same format applies: plate name top left, numbers along the top row to label columns, letters along the left column to label rows, and an empty row between plates.

In the top plate, every well is filled, but that's not necessary. Scroll down to the plate labeled Concentration. Here you see the same structure, but some of the wells are empty. Those wells will simply be represented as `NA`. If they're empty in every plate you're using, they'll be omitted.

### Tips for creating files

It's easy and convenient to make plate layouts like this in a spreadsheet program. Just make sure to save them as .csv files. One caution: some spreadsheet programs will include columns in the .csv output if they've ever had text in them, even if they're currently empty. So if you get errors about the plate layout being incorrect, but it looks right to you, try highlighting and deleting 10 columns to the right of the plate and 10 rows under the plate and re-saving the file.

Starting from scratch with `read_plate()`
-----------------------------------------

Commonly, an instrument will output data in the form of a plate and that's where you'll want to start. Take this data and copy and paste it into the format described above and save it as a .csv file. `read_plate()` will turn each plate in the file into a column, so add all the metadata you want as more plates below the data plate.

Below we illustrate getting the file path for the .csv file of interest and then reading it in. Note that we use `system.file()` here to get the file path of the example file installed with the package, but for your own files you would specify the file path relative to the current working directory without using `system.file()`.

``` r
bk <- system.file("extdata", "example-1.csv", package = "plateR")
   
data <- read_plate(
      file = bk,                    # full path to the .csv file
      well_ids_column = "Wells",    # name to give column of well IDs (optional)
      plate_size = 96               # total number of wells on the plate (optional)
)
str(data)
#> 'data.frame':    96 obs. of  6 variables:
#>  $ Wells           : chr  "A01" "A02" "A03" "A04" ...
#>  $ BacterialKilling: int  2 3 2 4 1 2 2 0 5 1 ...
#>  $ Viability       : int  99 99 99 97 99 93 95 98 99 94 ...
#>  $ Concentration   : num  0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 NA ...
#>  $ Sample          : chr  "Sample A" "Sample B" "Sample C" "Sample A" ...
#>  $ Treatment       : chr  "Drug A" "Drug A" "Drug A" "Drug B" ...

head(data)
#>   Wells BacterialKilling Viability Concentration   Sample Treatment
#> 1   A01                2        99          0.01 Sample A    Drug A
#> 2   A02                3        99          0.01 Sample B    Drug A
#> 3   A03                2        99          0.01 Sample C    Drug A
#> 4   A04                4        97          0.01 Sample A    Drug B
#> 5   A05                1        99          0.01 Sample B    Drug B
#> 6   A06                2        93          0.01 Sample C    Drug B
```

To check that the new data frame matched everything up as we expected, we can use `view_plate()`.

``` r
# check concentrations column
view_plate( 
      data = data,                         # data frame of interest
      well_ids_column = "Wells",           # name of the column with the well IDs
      column_to_display = "Concentration", # column to display 
      plate_size = 96                      # total number of wells on the plate (optional)
)   
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
view_plate(data, "Wells", "Sample")
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

Sometimes instruments give you data in the form you want: one row per well. But with complicated plate layouts, it can be a pain to match up metadata to the appropriate well. That's where `add_plate()` comes in: you take a data frame that already has one well per row and you add columns from a plate-shaped .csv file.

In this case, we'll imagine that the instrument that measured bacterial killing gave a file with one well per row ("example-2-data.csv"), but that the instrument that measured viability gave us the data in the form of a plate layout as before. We want to add all our metadata (concentration, sample, drug) in plate format for convenience.

First, read in the data you want to add columns to.

``` r
bk2 <- system.file("extdata", "example-2-data.csv", 
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

Now, we want to add the data from the plate layout file to this data frame and match it up by wells. Note that it doesn't matter what order the wells are in in `data2`: In the call to `add_plate` we'll specify which column in `data2` has the well IDs and it'll match it up that way.

``` r
meta <- system.file("extdata", "example-2-metadata.csv", package = "plateR")
data2 <- add_plate(
      file = meta,                # full paths to the .csv files
      data = data2,               # data frame to add to    
      well_ids_column = "Wells",  # name of column of well IDs in data frame
      plate_size = 96             # total number of wells on the plate (optional)
)
str(data2)
#> 'data.frame':    96 obs. of  6 variables:
#>  $ Wells           : Factor w/ 96 levels "A01","A02","A03",..: 1 2 3 4 5 6 7 8 9 10 ...
#>  $ BacterialKilling: int  2 3 2 4 1 2 2 0 5 1 ...
#>  $ Viability       : int  99 99 99 97 99 93 95 98 99 94 ...
#>  $ Concentration   : num  0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 NA ...
#>  $ Sample          : chr  "Sample A" "Sample B" "Sample C" "Sample A" ...
#>  $ Treatment       : chr  "Drug A" "Drug A" "Drug A" "Drug B" ...

head(data2)
#>   Wells BacterialKilling Viability Concentration   Sample Treatment
#> 1   A01                2        99          0.01 Sample A    Drug A
#> 2   A02                3        99          0.01 Sample B    Drug A
#> 3   A03                2        99          0.01 Sample C    Drug A
#> 4   A04                4        97          0.01 Sample A    Drug B
#> 5   A05                1        99          0.01 Sample B    Drug B
#> 6   A06                2        93          0.01 Sample C    Drug B
```
