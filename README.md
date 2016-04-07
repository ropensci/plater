<!-- README.md is generated from README.Rmd. Please edit that file -->
plateR
======

plateR makes it easy to work with data in the form of microtiter plate layouts.

Installation
------------

plateR is not currently on CRAN. You can install the latest GitHub version like this:

``` r
install.packages("devtools") # if you don't already have it
devtools::install_github("seaaan/plateR")
```

Getting your data in
--------------------

Plate readers, qPCR machines, and all manner of scientific instruments produce data in tabular form that mimics a microtiter plate: each cell corresponds to a well as physically laid out on the plate. Similarly, it's often easiest to keep records of what was what (control vs. treatment, concentration, sample type, etc.) in a similar plate layout form.

But data in those dimensions aren't ideal for analysis. That's where `read_plate()` and `add_plate()` come in.

-   `read_plate()` takes data in plate layout form and converts it to a data frame, with one well per row, identified by well name.
-   `add_plate()` does the same thing, but merges the new columns into an existing data frame you provide.

Now you can easily whip up some plate layouts describing the details of your experiment, and the experimental data itself, and pop it into a tidy form with no trouble.

To make it even easier, if you have multiple plates in an experiment, use `read_plates()` to read them all in and combine them into a single data frame.

Seeing your data
----------------

Sometimes it's useful to map your data back onto a plate (are the weird outliers all from the same corner of the plate?). For that, there's `view_plate()`, which takes a data frame with one well per row, and lays it out like it's on a plate.

Vignette
--------

For a detailed example of how to use `plateR`, check out <https://github.com/seaaan/plateR/blob/master/inst/doc/plateR-basics.md>

[![Travis-CI Build Status](https://travis-ci.org/seaaan/plateR.svg?branch=master)](https://travis-ci.org/seaaan/plateR)
