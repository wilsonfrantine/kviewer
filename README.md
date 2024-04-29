# Kviewer
The simplest way to visualizing Structure results and calculate K-estimators in R.

This package provide a straightforward way to checkout the results from your runs on Structure.
By now, it can calculate the estimators from Evanno methods (Evanno et al. 2005) and Pritchard's linearization of lnPr(X|M), details in Pritchard (2000) and Structure documentation.

## How to use the package

### Installing

`remotes::install_github("wilsonfrantine/kviewer")`

### Running
Load the library:

`library("kviewer")`

Then run:

`results <- kview()`

Select one of the structure results files ("your_run_f").

If you wanna provide pop names for plotting:

`kview(pop_names=c("mypop1", "Pop from 2", "and so on..."))`

### Help

for more details, use:

`?kview`

Any crash reports here or for wilsofrantine@gmail.com
Feel free to implement the code in your scripts.
