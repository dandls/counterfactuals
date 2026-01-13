# counterfactuals 1.0.0
* Add DOI of JSS publication and change CITATION
* Minor style adjustments in references
* Update of vignettes according to JSS publication

# counterfactuals 0.1.6 
* Fix display issues in html vignettes

# counterfactuals 0.1.5
* Compatability with name changes in `bbotk` (OptimInstanceMultiCrit)

# counterfactuals 0.1.4
* Compatibility with upcoming 'paradox' release
* Improved print methods for counterfactual methods

# counterfactuals 0.1.3
* Resolved bug in `Counterfactuals$evaluate(show_diff = TRUE)` after `$subset_to_valid()` and `$revert_subset_to_valid()` were called.
* Throw errors if `x_nn_correct = TRUE` but no correctly classified observation available. 
* Add required packages for running MOCClassif and MOCRegr with `use_conditional_mutator = TRUE`.
* Add concept index entries to facilitate searching for class methods via `help.search()`. 
  
# counterfactuals 0.1.2
* Updated the package vignettes. Instead of the pima diabetis and the Boston 
housing datasets, the German credit and a plasma retinol datasets are now used.
* Extended the $plot_parallel() method to allow all feature types, not only numeric ones.
Also the style was slightly adapted.

# counterfactuals 0.1.1
Initial release
