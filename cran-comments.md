# streamDepletr 0.2.0

## Resubmission
This is a resubmission to remove dependencies on sp, rgeos, and rgdal due to 
the anticipated deprecation of these packages. streamDepletr now relies primarily
on sf for spatial operations.

## Submission notes
This submission is intended to address the issues described by rsbivand on 
the following GitHub issue: https://github.com/FoundrySpatial/streamDepletr/issues/19

## Test environments
* local windows 10 install, R 4.2.2
* win-builder (devel and release)
* Windows Server 2022, R-devel, 64 bit (on r-hub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (on r-hub)
* Fedora Linux, R-devel, clang, gfortran (on r-hub)

## R CMD check results
### Windows on win-builder
0 ERRORs | 0 WARNINGs | 0 NOTES

### Windows on r-hub
0 ERRORs | 0 WARNINGs | 2 NOTES

Both notes were related to `lastMiKTeXException`: 
```
* checking for non-standard things in the check directory ... NOTE
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
This [appears to be a bug in r-hub and does not require action](https://github.com/r-hub/rhub/issues/503).

### Linux (Ubuntu and Fedora on R-hub)
0 ERRORs | 0 WARNINGs | 1 NOTES
(same on Ubuntu and Fedora)
```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```
This is an r-hub issue and therefore does not require action.

## Downstream dependencies
There are currently no downstream dependencies for this package.

# streamDepletr 0.1.2

## Submission notes
This submission updates the CRS for `stream_lines` to address a stale sp data 
object issue following instructions sent by Roger Bivand.

# streamDepletr 0.1.1

## Resubmission
This is a resubmission. In this version I have:
* Fixed a broken URL in readme.md.

## Submission notes
This submission is intended to address a dataset dependency issue
 noted by Brian Ripley.

## Test environments
* local windows 10 install, R 3.6.3
* win-builder (devel and release)
* Windows Server 2008 R2 SP1 (on R-hub), R-devel, 32/64 bit
* Ubuntu Linux 16.04 LTS (on R-hub), R-release, GCC
* Fedora Linux (on R-hub), R-devel, clang, gfortran

## R CMD check results
### Windows on win-builder
0 ERRORs | 0 WARNINGs | 1 NOTES

There was 1 note:
* checking CRAN incoming feasibility ... NOTE
Some potentially mis-spelled words in DESCRIPTION are suggested;
 however, the spelling is correct.

There is a note regarding the data() dependency which caused the previous
version to be removed from CRAN. This is corrected in this release.

### Linux (Ubuntu and Fedora on R-hub)
0 ERRORs | 0 WARNINGs | 2 NOTES

There were 2 notes:
* checking CRAN incoming feasibility ... NOTE
Some potentially mis-spelled words in DESCRIPTION are suggested;
 however, the spelling is correct.

There is a note regarding the data() dependency which caused the previous
version to be removed from CRAN. This is corrected in this release.

* checking examples ... NOTE
Examples with CPU or elapsed time > 5s
                   user system elapsed
apportion_polygon 2.612   2.92   5.531

## Downstream dependencies
There are currently no downstream dependencies for this package.

# streamDepletr 0.1.0

## Resubmission
This is a resubmission. In this version I have:
* Added the copyright holder, Foundry Spatial Ltd., to the Authors@R field of DESCRIPTION

## Test environments
* local windows 10 install, R 3.5.3
* ubuntu 14.04 (on travis-ci), R 3.5.3
* win-builder (devel and release)
* Windows Server 2008 R2 SP1 (on R-hub), R-devel, 32/64 bit
* macOS 10.11 El Capitan (on R-hub), R-release

## R CMD check results
0 ERRORs | 0 WARNINGs | 1 NOTES.

When checking on win-builder, some potentially mis-spelled words in 
DESCRIPTION are suggested; however, the spelling is correct.

## Downstream dependencies
There are currently no downstream dependencies for this package.