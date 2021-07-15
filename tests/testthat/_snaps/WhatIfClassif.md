# Correct handling of lower and upper

    Code
      WhatIfClassif$new(iris_pred, n_counterfactuals = 30, lower = c(Sepal.Length = 5.5),
      upper = c(Sepal.Length = 5.7))
    Warning <simpleWarning>
      Could only find 21 candidate(s) with feature values between `lower` and `upper`.
    Output
      Counterfactual explanation method:  WhatIfClassif 
      Parameters:
       - n_counterfactuals:  30

---

    Code
      WhatIfClassif$new(iris_pred, n_counterfactuals = n, lower = c(Sepal.Length = 0),
      upper = c(Sepal.Length = 1))
    Warning <simpleWarning>
      Could only find 0 candidate(s) with feature values between `lower` and `upper`.
    Output
      Counterfactual explanation method:  WhatIfClassif 
      Parameters:
       - n_counterfactuals:  5

---

    Code
      WhatIfClassif$new(iris_pred, n_counterfactuals = n, lower = c(Sepal.Length = 100),
      upper = c(Sepal.Length = 200))
    Warning <simpleWarning>
      Could only find 0 candidate(s) with feature values between `lower` and `upper`.
    Output
      Counterfactual explanation method:  WhatIfClassif 
      Parameters:
       - n_counterfactuals:  5

