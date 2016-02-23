So what's the point?
--------------------

You do your experiment in a plate. You think about which wells are which in terms of that plate (these wells were Drug A, these wells were Drug B, etc.). Maybe the data even comes out shaped like a plate.

But that's a terrible shape for data analysis. You want one well per row. Trying to match up your mental picture of the plate to a column of well IDs is a huge pain though. The point of `plateR` is to let you store your data shaped like a plate and then turn it into a tidy form for data analysis effortlessly.

An example
----------

You do an experiment. Your data look like this:

    #>    1  2  3  4  5  6  7  8  9 10 11 12
    #> A  2  3  2  4  1  2  2  0  5  1  0  0
    #> B  2  3  7  6  6  9  8 10  7  3  0  2
    #> C 12 11 11 59 58 71  8  8  0  3  5  0
    #> D 16 19 17 98 81 90 10 13 12  2  4  3
    #> E 61 53 70 91 91 80 15 13 12  5  2  1
    #> F 95 80 99 98 82 89 16 19 17  4  3  5
    #> G 83 84 83 80 87 87 75 61 54  5  1  3
    #> H 83 83 80 85 94 93 84 85 91  5  5  2

Each sample got treated with a drug, in this pattern:

    #>   1 2 3 4 5 6 7 8 9   10   11   12
    #> A A A A B B B C C C Ctrl Ctrl Ctrl
    #> B A A A B B B C C C Ctrl Ctrl Ctrl
    #> C A A A B B B C C C Ctrl Ctrl Ctrl
    #> D A A A B B B C C C Ctrl Ctrl Ctrl
    #> E A A A B B B C C C Ctrl Ctrl Ctrl
    #> F A A A B B B C C C Ctrl Ctrl Ctrl
    #> G A A A B B B C C C Ctrl Ctrl Ctrl
    #> H A A A B B B C C C Ctrl Ctrl Ctrl

When you analyze it, you want it to look like this:

    #>   Wells BacterialKilling Treatment
    #> 1   A01                2         A
    #> 2   A02                3         A
    #> 3   A03                2         A
    #> 4   A04                4         B
    #> 5   A05                1         B
    #> 6   A06                2         B

`plateR` makes that effortless.

File structure: `plateR` format
-------------------------------

Let's get started.

When you installed `plateR`, several example .csv files were installed. These files illustrate how `plateR` format files should be formatted. To find out where the files are stored on your computer, run this code.

``` r
# get the file path to the folder
library(plateR)
system.file("extdata", package = "plateR")
#> [1] "C:/Users/smhughes/Documents/R/win-library/3.2/plateR/extdata"
```

Now, open up the appropriate folder on your computer. You will see three files:

-   `example-1.csv`
-   `example-2-part-A.csv`
-   `example-2-part-B.csv`

Open `example-1.csv` in a spreadsheet application. As you can see, the file is formatted as several microtiter plates. This is `plateR` format.

The important features of `plateR` format are:

-   Top-left most cell of each plate holds the name of the plate (to be used as the column title after conversion to a data frame)
-   The rest of the top row is labeled 1-12.
-   The rest of the first column is labeled A-H.
-   Plates are separated by an empty row.

Wells can be empty. In the top layout, every well is filled. Scroll down to the plate labeled Concentration. Here you see the same structure, but some of the wells are empty. Those wells will be represented as `NA`. If they're empty in every plate in the file, they'll be omitted.

The examples are 96-well, but any standard size plate (12-384 wells) can be used in `plateR` format: plate name top left, numbers along the top row to label columns, letters along the left column to label rows, and an empty row between plates.

### Tips for creating files

The idea is that you store your data in `plateR` format and then use `plateR` to convert it into a useful data frame. Each plate layout in the file will be converted to a column and each well will be represented as a row, with values in each column corresponding to the values at that well in the plate layouts.

To create a file in this format, copy and paste your data into a spreadsheet program and then continue creating more plate-shaped layouts below it, with one layout for each column you want in your data frame. Fill in the wells with the appropriate information. Wells can either contain data or annotations, so if you measured more than one thing about each well, just create one plate-layout (in the same file) for each measurement. Similarly, for each variable you want to record about a well (sample source, treatment, experiment/control, etc.), add another plate layout to the file.

One caution: some spreadsheet programs will include columns in the .csv output if they've ever had text in them, even if they're currently empty. So if you get errors about the plate layout being incorrect, but it looks right to you, try deleting 10 columns to the right of the plate and 10 rows under the plate and re-saving the file.

Approach 1: Starting from scratch with `read_plate()`
-----------------------------------------------------

Let's say you did the experiment shown in `example-1.csv`. The instrument measuring bacterial killing gave you data shaped like a plate and you combined it with information about each well into `example-1.csv`.

But what you really want is a data frame with columns for each variable, matched up by well. That's what `read_plate()` is for.

``` r
bk <- system.file("extdata", "example-1.csv", package = "plateR")
   
data <- read_plate(
      file = bk,                    # full path to the .csv file
      well_ids_column = "Wells",    # name to give column of well IDs (optional)
      plate_size = 96               # total number of wells on the plate (optional)
)
str(data)
#> 'data.frame':    96 obs. of  5 variables:
#>  $ Wells           : chr  "A01" "A02" "A03" "A04" ...
#>  $ BacterialKilling: int  2 3 2 4 1 2 2 0 5 1 ...
#>  $ Concentration   : num  0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 NA ...
#>  $ Sample          : chr  "Sample A" "Sample B" "Sample C" "Sample A" ...
#>  $ Treatment       : chr  "Drug A" "Drug A" "Drug A" "Drug B" ...

head(data)
#>   Wells BacterialKilling Concentration   Sample Treatment
#> 1   A01                2          0.01 Sample A    Drug A
#> 2   A02                3          0.01 Sample B    Drug A
#> 3   A03                2          0.01 Sample C    Drug A
#> 4   A04                4          0.01 Sample A    Drug B
#> 5   A05                1          0.01 Sample B    Drug B
#> 6   A06                2          0.01 Sample C    Drug B
```

Note that we use `system.file()` here to get the file path of the example file installed with the package, but for your own files you would specify the file path relative to the current working directory without using `system.file()`.

So, now you're done: you have a tidy data frame that's easy to analyze.

Approach 2: Combine two files with `add_plate()`
------------------------------------------------

The above approach works when all of your data are plate-shaped. Sometimes, though, instruments give you data with one row per well. But you probably think about your experiment in terms of what well was what, and it can be a pain to figure out just from the well ID. The purpose of `add_plate()` is to combine two spreadsheets: one that has one well per row and another that has plate-shaped data.

Let's imagine you did the same experiment as above, except that the instrument that measured bacterial killing gave a file with one well per row (`example-2-part-B.csv`). The annotations, with information about what was in each well (concentration, sample, drug), are easiest to think about as plate-shaped data like before (`example-2-part-B.csv`).

You want to combine those two files, matching up the wells.

First, read in the bacterial killing data.

``` r
bk2 <- system.file("extdata", "example-2-part-A.csv", 
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
meta <- system.file("extdata", "example-2-part-B.csv", package = "plateR")
data2 <- add_plate(
      file = meta,                # full path to the .csv file
      data = data2,               # data frame to add to    
      well_ids_column = "Wells",  # name of column of well IDs in data frame
      plate_size = 96             # total number of wells on the plate (optional)
)
str(data2)
#> 'data.frame':    96 obs. of  5 variables:
#>  $ Wells           : Factor w/ 96 levels "A01","A02","A03",..: 1 2 3 4 5 6 7 8 9 10 ...
#>  $ BacterialKilling: int  2 3 2 4 1 2 2 0 5 1 ...
#>  $ Concentration   : num  0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 NA ...
#>  $ Sample          : chr  "Sample A" "Sample B" "Sample C" "Sample A" ...
#>  $ Treatment       : chr  "Drug A" "Drug A" "Drug A" "Drug B" ...

head(data2)
#>   Wells BacterialKilling Concentration   Sample Treatment
#> 1   A01                2          0.01 Sample A    Drug A
#> 2   A02                3          0.01 Sample B    Drug A
#> 3   A03                2          0.01 Sample C    Drug A
#> 4   A04                4          0.01 Sample A    Drug B
#> 5   A05                1          0.01 Sample B    Drug B
#> 6   A06                2          0.01 Sample C    Drug B
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
