library(ranger)
# remotes::install_github("bips-hb/arf") # required for conditional sampling
load_all()
set.seed(2024)

data(german, package = "rchallenge")  
credit = as.data.table(german[, c("duration", "amount", "purpose", "age", "employment_duration", "housing", "number_credits", "credit_risk")])
credit[, duration := as.numeric(duration)]
credit[, amount := as.numeric(amount)]
credit[, age := as.numeric(age)]

idx <- 998L
x_interest = credit[idx, ]

target_prob <- 0.6
feats_to_change <- 1

# Fit model without obs of interest
rf <- ranger(credit_risk ~ ., credit[-idx, ], probability = TRUE)
predict(rf, x_interest)$prediction
predictor = Predictor$new(model = rf, data = credit[-idx,], y = "credit_risk")

cac = CountARFactualClassif$new(predictor = predictor, 
  max_feats_to_change = 2L, importance_method = "icesd", 
  feature_selector = "importance")
cfexp = cac$find_counterfactuals(
  x_interest, desired_class = "good", desired_prob = c(0.6, 1)
)
cfexp$evaluate_set()
cfexp$plot_surface(c("duration", "amount"))
