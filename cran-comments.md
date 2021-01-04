# Update, version 1.0.3, 4 Jan 2021

This is a minor update: 

* Fix bug where class of some objects was mishandled
* Replace internal use of deprecated function

## Test environments
* ubuntu 18.04 on travis-ci:  devel   2021-01-02 r79767 
                              release 4.0.2 (2020-06-22)
* win-builder:                devel   2021-01-02 r79767
                              release 4.0.3 (2020-10-10)
* rhub                        
    * ubuntu 16.04            3.6.1
    * fedora                  2020-10-24 r79367
    * windows                 2020-12-14 r79633

## R CMD check results
There were no ERRORs or WARNINGs. 

There was one NOTE:

   * checking CRAN incoming feasibility ... NOTE_to_CRAN_maintainers
   Maintainer: 'Sean Hughes <smhughes@uw.edu>'

## Downstream dependencies
There are currently no downstream dependencies for this package.