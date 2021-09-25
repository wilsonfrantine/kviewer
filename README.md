# Kviewer
The simplest way to visualizing Structure results and calculate K-estimators in R.

This package provide a straightforward way to checkout the results from your runs on Structure.
At the moment we can calculate the estimators from Evanno methods (Evanno et al. 2005) and Pritchard's linearization of lnPr(X|M), details in Pritchard (2000) and Structure documentation.

## How to use the package

### Installing
You must first install the package with the following command:

`devtools::install_github("wilsonfrantine/kviewer")`

### Running
It is simple like this:

`library("kviewer")`

Then:

`kview()`

Pickup the one of the structure results files ("your_run_f") or a zip file containing the structure outputs.

you may also provide pop names for plotting:

`kview(pop_names=c("mypop1", "Pop from 2", "and so on..."))`

Enjoy
