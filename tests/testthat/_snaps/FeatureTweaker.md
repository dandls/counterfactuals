# Initialization returns appropriate error message if not a classification task

    `FeatureTweaker` only works for classification tasks.

# Initialization returns appropriate error message if randomForest.formula class

    `FeatureTweaker` cannot be applied to randomForest models specified with a formula.

# Initialization returns appropriate error message if model is not randomForest

    `FeatureTweaker` only works for randomForest models.

# Initialization returns appropriate error message if training data are not standardized

    `FeatureTweaker` can only handle standardized features in training data.

# Returns error message if categorical variables are in training data

    `FeatureTweaker` cannot handle categorical variables in the training data.

# Init works for classification tasks only

    `FeatureTweaker` only works for classification tasks.

# `desired_outcome` is required for multiclass

    The `desired_outcome` has to be specified for multiclass classification tasks.

