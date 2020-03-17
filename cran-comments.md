# Update, version 1.0.2, 17 Mar 2020

This is a minor update:

* Changes to tests to comply with new CRAN policy on `data.frame(..., stringsAsFactors = FALSE)`
* Add support for 6- and 1536-well plates
* Change behavior of add_plate so that when the plate layout contains more wells than the input data frame, those wells are appended to the end of the data frame instead of erroring. 

## Test environments
* ubuntu 16.04 on travis-ci:  devel   r77873 (2020-02-28) 
                              release 3.6.2  (2017-01-27)
* win-builder:                devel   r77925 (2020-03-11)
                              release 3.6.3  (2020-02-29)
* local Windows 10:           release 3.5.2  (2018-12-20)
* rhub                        devel   r77917 (2020-03-08)   

## R CMD check results
There were no ERRORs or WARNINGs. 

There was one NOTE:

   * checking CRAN incoming feasibility ... NOTE_to_CRAN_maintainers
   Maintainer: 'Sean Hughes <smhughes@uw.edu>'

## Downstream dependencies
There are currently no downstream dependencies for this package.