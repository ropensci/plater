File structure: `plateR` format
-------------------------------

When you installed `plateR`, several example .csv files were installed. These files illustrate how `plateR` format files should be formatted. To find out where the files are stored on your computer, run this code.

``` r
# get the file path to the folder
library(plateR)
system.file("extdata", package = "plateR")
#> [1] "C:/Users/smhughes/Documents/R/win-library/3.2/plateR/extdata"
```

Now, open up the appropriate folder on your computer. You will see three files:

-   `example-1.csv`
-   `example-2-data.csv`
-   `example-2-metadata.csv`

Open `example-1.csv` in a spreadsheet application. As you can see, the file is formatted as several microtiter plates. The important features of `plateR` format are:

-   Top-left most cell of each plate holds the name of the plate (to be used as the column title after conversion to a data frame)
-   The rest of the top row is labeled 1-12.
-   The rest of the first column is labeled A-H.
-   Plates are separated by an empty row.

Wells can be empty. In the top plate, every well is filled. Scroll down to the plate labeled Concentration. Here you see the same structure, but some of the wells are empty. Those wells will be represented as `NA`. If they're empty in every plate in the file, they'll be omitted.

The examples are 96-well, but any standard size plate (12-384 wells) can be used in `plateR` format: plate name top left, numbers along the top row to label columns, letters along the left column to label rows, and an empty row between plates.

### Tips for creating files

It's easy and convenient to make `plateR` format files in a spreadsheet program. Just make sure to save them as .csv files.

One caution: some spreadsheet programs will include columns in the .csv output if they've ever had text in them, even if they're currently empty. So if you get errors about the plate layout being incorrect, but it looks right to you, try deleting 10 columns to the right of the plate and 10 rows under the plate and re-saving the file.

Approach 1: Starting from scratch with `read_plate()`
-----------------------------------------------------

Let's say you did the experiment shown in `example-1.csv`. The instruments measuring bacterial killing and viability gave you data shaped like a plate and you combined them, along with information about what each plate represents, into `example-1.csv`.

But what you really want is a data frame with columns for each variable, matched up by well. That's what `read_plate()` is for.

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

Note that we use `system.file()` here to get the file path of the example file installed with the package, but for your own files you would specify the file path relative to the current working directory without using `system.file()`.

So, now you're done: you have a tidy data frame that's easy to analyze.

Approach 2: Combine two files with `add_plate()`
------------------------------------------------

The above approach works when all of your data are plate-shaped. Sometimes, though, instruments give you data with one row per well. But you probably think about your experiment in terms of what well was what, and it can be a pain to figure out just from the well ID. That's where `add_plate()` comes in: you combine two spreadsheets: one that has one well per row and another that has plate-shaped data.

Let's imagine you did the same experiment as above, except that the instrument that measured bacterial killing gave a file with one well per row (`example-2-data.csv`), while the instrument that measured viability gave us plate-shaped data like before (`example-2-metadata.csv`). In the second file, you also have the data labeling what well was what (concentration, sample, drug).

You want to combine those two files, matching up the wells.

First, read in the bacterial killing data.

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

Now, you want to combine `data2` with all the plate-shaped data and match it up by well. That's where `add_plate()` comes in. It takes a .csv file in `plateR` format and combines it with a one-well-per-row data frame.

``` r
meta <- system.file("extdata", "example-2-metadata.csv", package = "plateR")
data2 <- add_plate(
      file = meta,                # full path to the .csv file
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

Note that it doesn't matter what order the wells are in in `data2`: In the call to `add_plate()` the column in `data2` with the well IDs is specified and it'll match it up using the well IDs.

So, now you're done: you have a tidy data frame that's easy to analyze.

Seeing is believing
-------------------

Sometimes it's useful to look at data as though it's back on the plate. That's where `view_plate()` comes in.

``` r
view_plate( 
      data = data2,                         # data frame of interest
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
```
