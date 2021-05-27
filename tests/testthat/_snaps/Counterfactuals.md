# $initialize() returns error if predictor given does not have the correct class

    Assertion on 'predictor' failed: Must inherit from class 'Predictor', but has class 'character'.

# $private$check_x_interest() returns error if x_interest column do not match X of predictor

    `x_interest` is invalid.
    x `x_interest` and `predictor$data$X` must have the same columns.
    i     x_interest     | predictor$data$X     
    [1] "Sepal.Length" - "cyl"            [1] 
    [2] "Sepal.Width"  - "disp"           [2] 
    [3] "Petal.Length" - "hp"             [3] 
    [4] "Petal.Width"  - "drat"           [4] 
    [5] "Species"      - "wt"             [5] 
                       - "qsec"           [6] 
                       - "vs"             [7] 
                       - "am"             [8] 
                       - "gear"           [9] 
                       - "carb"           [10]

# $private$check_x_interest() returns error if x_interest column types do not match types of X of predictor

    `x_interest` is invalid.
    x `x_interest` and `predictor$data$X` must have the same column types.
    i      x_interest | predictor$data$X     
     [5] "double"   | "double"         [5] 
     [6] "double"   | "double"         [6] 
     [7] "double"   | "double"         [7] 
     [8] "integer"  -                      
     [9] "double"   | "double"         [8] 
    [10] "double"   | "double"         [9] 
                    - "double"         [10]

# $private$check_x_interest() returns error if x_interest feature values are outside range of predictor data

    `x_interest` is invalid.
    x Feature values of `x_interest` outside of range of `predictor$data$X` or given arguments `lower` or `upper`.
    i Please modify arguments `lower` or `upper` accordingly.

# $plot_surface() returns error message if `feature_names` are not in data

    `feature_names` is invalid.
    x The `feature_names` are not in the training data.
    i The colnames of the training data are: 'col_a', 'col_b', 'col_c'.
    i `feature_names` are: 'not_in_data', 'col_b'.

# $get_freq_of_feature_changes returns error if there are not results yet.

    There are no results yet.
    i Please run `$find_counterfactuals()` first.

