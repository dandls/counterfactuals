# Correct handling of lower and upper

    Code
      WhatIfRegr$new(pred, n_counterfactuals = n, lower = c(disp = 80, hp = 100),
      upper = c(disp = 100, hp = 120))
    Warning <simpleWarning>
      Could only find 1 candidate(s) with feature values between `lower` and `upper`.
    Output
      Counterfactual explanation method:  WhatIfRegr 
      Parameters:
       - n_counterfactuals:  3

---

    Code
      WhatIfRegr$new(pred, n_counterfactuals = n, lower = c(disp = 0), upper = c(
        disp = 10))
    Warning <simpleWarning>
      Could only find 0 candidate(s) with feature values between `lower` and `upper`.
    Output
      Counterfactual explanation method:  WhatIfRegr 
      Parameters:
       - n_counterfactuals:  3

---

    Code
      WhatIfRegr$new(pred, n_counterfactuals = n, lower = c(disp = 1000), upper = c(
        disp = 2000))
    Warning <simpleWarning>
      Could only find 0 candidate(s) with feature values between `lower` and `upper`.
    Output
      Counterfactual explanation method:  WhatIfRegr 
      Parameters:
       - n_counterfactuals:  3

