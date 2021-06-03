# $initialize() returns error if predictor given does not have the correct class

    Assertion on 'predictor' failed: Must inherit from class 'Predictor', but has class 'character'.

# $initialize() returns error if lower or upper contain names that are not columns in predictor$data$X

    Assertion on 'all(names(lower) %in% names(predictor$data$X))' failed: Must be TRUE.

---

    Assertion on 'all(names(upper) %in% names(predictor$data$X))' failed: Must be TRUE.

# $initialize() returns error if lower is greater than upper

    Assertion on 'lower <= upper' failed: Must be TRUE.

