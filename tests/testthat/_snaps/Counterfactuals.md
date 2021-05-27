# $initialize() returns error if predictor given does not have the correct class

    Assertion on 'predictor' failed: Must inherit from class 'Predictor', but has class 'character'.

# $plot_surface() returns error message if `feature_names` are not in data

    `feature_names` is invalid.
    x The `feature_names` are not in the training data.
    i The colnames of the training data are: 'col_a', 'col_b', 'col_c'.
    i `feature_names` are: 'not_in_data', 'col_b'.

# $get_freq_of_feature_changes returns error if there are not results yet.

    There are no results yet.
    i Please run `$find_counterfactuals()` first.

