# Update, version 1.0.5, 11 Feb 2022

This is a minor update: 

* Belatedly add package \alias to plater.R per CRAN request.
* Fix possible problem identified by R CMD CHECK

## Test environments
* github        macOS 14.7      R release 4.4.1 (2024-06-14)
* github        Windows 10      R release 4.4.1 (2024-06-14)
* github        Ubuntu 22.04    R release 4.4.1 (2024-06-14)
* github        Ubuntu 22.04    R release 4.3.3 (2024-02-29)
* github        Ubuntu 22.04    R devel (2024-10-22 r87264)
* win-builder   Windows         R release 4.1.2 (2021-11-01)
* win-builder   Windows         R devel (2022-02-10 r81713 ucrt)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was one NOTE:

   * checking CRAN incoming feasibility ... NOTE_to_CRAN_maintainers
   Maintainer: 'Sean Hughes <smhughes@uw.edu>'

## Downstream dependencies
We checked 1 reverse dependency (0 from CRAN + 1 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages