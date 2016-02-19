## Test environments
* local Windows 7, R 3.2.3
* local Ubuntu 14.04, R 3.2.2
* ubuntu 12.04 (on travis-ci), R 3.2.3
* win-builder (devel (2016-02-18 r70185) and release (3.2.3 Patched (2016-02-04 r70085)))

## R CMD check results
There were no ERRORs or WARNINGs. 

There was one NOTE, indicating that this is a new submission. 

   * checking CRAN incoming feasibility ... NOTE
   Maintainer: 'Sean Hughes <smhughes@uw.edu>'
   New submission

   Version contains large components (0.2.0.9000)
   
   Possibly mis-spelled words in DESCRIPTION:
     Microtiter (2:43)
     microtiter (6:61)
     
     
I think large components means the .9000 (should be excluded from submission)
Both microtiter and microwell come up as "mis-spelled"
     
## Downstream dependencies
There are currently no downstream dependencies for this package.