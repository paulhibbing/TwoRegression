## Resubmission:
This is a resubmission with a major version increment
due to the following changes:

* Added support for other pre-existing models in addition to those
  of Hibbing et al. (2018, *Med Sci Sports Exerc*)
* Established unified means of implementing pre-existing models through
  a single `TwoRegression` function
* Added functions to facilitate development and application of new models


## Test environments
* local Windows 10 install, R 4.0.5
* ubuntu 20.04.1 (on R-hub, R-release)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Paul R. Hibbing <paulhibbing@gmail.com>'

Possibly mis-spelled words in DESCRIPTION:
  Crouter (15:43)
  Hibbing (15:5)
  Kaplan (15:30)
  LaMunion (15:17)

The "possibly mis-spelled words" are author names
pertaining to the reference in DECRIPTION.

## Downstream dependencies
There are no reverse dependencies for TwoRegression.
