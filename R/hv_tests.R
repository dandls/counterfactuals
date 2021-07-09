# mydf = mtcars
# mydf$am = as.factor(mydf$am)
# mydf$vs = as.factor(mydf$vs)
# rf = randomForest::randomForest(am ~ ., data = mydf, ntree = 5L)
# pred = Predictor$new(rf, data = mydf, type = "class")
# mocc = MOCClassif$new(pred, n_generations = 5L)
# x_interest = head(subset(mydf, select = -am), n = 1L)
# cfactuals = quiet(mocc$find_counterfactuals(x_interest, desired_class = "1"))
#
# this_data = mocr$optimizer$archive$data[, c("batch_nr", "dist_target", "dist_x_interest", "nr_changed", "dist_train")]
#
# ref_point = c(min(abs(mod$predict(x_interest) - c(0.5, 1))), 1, ncol(x_interest), 1)
#
# for (i in 1:max(this_data$batch_nr)) {
#   print(ecr::computeHV(as.matrix(this_data[batch_nr == i, -"batch_nr"])))
# }
#
#
# for (i in 1:max(this_data$batch_nr)) {
#   print(miesmuschel:::domhv(as.matrix(this_data[batch_nr == 1, -"batch_nr"])))
# }


# set.seed(5748554)
# library(randomForest)
# data("Boston", package  = "MASS")
# rf =  randomForest(medv ~ ., data = Boston)
# X = Boston[-which(names(Boston) == "medv")]
# mod = Predictor$new(rf, data = X)
# 
# # Then we explain the prediction of the first instance with the
# # Counterfactuals method
# x_interest = X[1,]
# target = 30
# mocr = MOCRegr$new(mod, n_generations = 10, epsilon = 0.5)
# cfactuals = mocr$find_counterfactuals(x_interest, target)
# this_data = mocr$optimizer$archive$data[, c("batch_nr", "dist_target", "dist_x_interest", "nr_changed", "dist_train")]
# 
# ref_point = c(min(abs(mod$predict(x_interest) - target)), 1, ncol(x_interest), 1)
# 
# for (i in 1:max(this_data$batch_nr)) {
#   print(ecr::computeHV(t(as.matrix(this_data[batch_nr == i, -"batch_nr"])), ref_point))
# }
# 
# 
# for (i in 1:max(this_data$batch_nr)) {
#   print(miesmuschel:::domhv(t(as.matrix(this_data[batch_nr == i, -"batch_nr"]))))
# }
# 
# for (i in 1:max(this_data$batch_nr)) {
#   print(emoa::dominated_hypervolume(t(as.matrix(this_data[batch_nr == i, -"batch_nr"])), ref = ref_point))
# }
# 
# 
# # Reprex
# library(miesmuschel)
# library(iml)
# library(randomForest)
# library(data.table)
# make_param_set = function(dt, lower, upper) {
#   param_list = lapply(names(dt), function(col_name){
#     column = dt[[col_name]]
# 
#     # lower bound
#     if (col_name %in% names(lower)) {
#       lb = lower[[col_name]]
#     } else {
#       lb = ifelse(is.numeric(column), min(column, na.rm = TRUE), NA)
#     }
# 
#     # upper bound
#     if (col_name %in% names(upper)) {
#       ub = upper[[col_name]]
#     } else {
#       ub = ifelse(is.numeric(column), max(column, na.rm = TRUE), NA)
#     }
# 
#     # make param
#     if (is.double(column)) {
#       param = paradox::ParamDbl$new(col_name, lower = lb, upper = ub)
#     } else if (is.integer(column)) {
#       param = paradox::ParamInt$new(col_name, lower = lb, upper = ub)
#     } else {
#       if (is.character(column)) {
#         levels = unique(column)
#       } else {
#         levels = levels(column)
#       }
#       param = paradox::ParamFct$new(col_name, levels = levels)
#     }
# 
#     param
#   })
# 
#   paradox::ParamSet$new(param_list)
# }
# 
# data(BostonHousing, package  = "mlbench")
# data = setDT(BostonHousing)[, chas := as.numeric(BostonHousing$chas)]
# 
# rf =  randomForest(medv ~ ., data = data)
# mod = Predictor$new(rf, data = data, y = "medv")
# x_interest = as.data.table(data[1, -ncol(data)])
# target = 30
# param_set = make_param_set(data[, (names(x_interest))], lower = NULL, upper = NULL)
# 
# codomain = ParamSet$new(list(
#   ParamDbl$new("dist_target", tags = "minimize"),
#   ParamDbl$new("dist_x_interest", tags = "minimize"),
#   ParamInt$new("nr_changed", tags = "minimize"),
#   ParamDbl$new("dist_train", tags = "minimize")
# ))
# 
# make_fitness_function = function(predictor, x_interest, param_set, target) {
#   function(xdt) {
#     dist_target = sapply(mod$predict(xdt)[[1L]], function(x) ifelse(between(x, target[1L], target[2L]), 0, min(abs(x - target))))
#     dist_x_interest = as.vector(StatMatch::gower.dist(x_interest, xdt, KR.corr = FALSE))
#     nr_changed = rowSums(xdt != x_interest[rep(seq_len(nrow(x_interest)), nrow(xdt)), ])
#     dist_train = apply(StatMatch::gower.dist(mod$data$X, xdt), 2L, FUN = function(dist) mean(sort(dist)[1]))
#     data.table(cbind(dist_target, dist_x_interest, nr_changed, dist_train))
#   }
# }
# fitness_function = make_fitness_function(mod, x_interest, param_set, target)
# 
# objective = bbotk::ObjectiveRFunDt$new(fun = fitness_function, domain = param_set, codomain = codomain)
# oi = bbotk::OptimInstanceMultiCrit$new(objective, terminator = bbotk::trm("gens", generations = 10))
# op_m = mut("gauss")
# op_r = rec("xounif")
# op_parent = sel("best")
# op_survival = sel("best", scl("nondom"))
# 
# mies_prime_operators(
#   search_space = oi$search_space, mutators = list(op_m), recombinators = list(op_r),
#   selectors = list(op_parent, op_survival)
# )
# mies_init_population(inst = oi, mu = 50)
# 
# tryCatch({
#   repeat {
#     offspring = mies_generate_offspring(oi, lambda = 50, op_parent, op_m, op_r)
#     mies_evaluate_offspring(oi, offspring)
#     mies_survival_plus(oi, 50, op_survival)
#   }
# }, terminated_error = function(cond) {
# })
# bbotk::assign_result_default(oi)
# 
# # Infinite recursion?
# fitnesses = oi$archive$data[, c("batch_nr", "dist_target", "dist_x_interest", "nr_changed", "dist_train")]
# for (i in 1:max(fitnesses$batch_nr)) {
#   print(miesmuschel:::domhv(as.matrix(fitnesses[batch_nr == i, -"batch_nr"]), nadir = NULL))
# }
# 
# # This works
# for (i in 1:max(fitnesses$batch_nr)) {
#   print(ecr::computeHV(t(as.matrix(fitnesses[batch_nr == i, -"batch_nr"])), ref.point = ref_point))
# }
# 
# 
# for (i in 1:max(fitnesses$batch_nr)) {
#   print(emoa::dominated_hypervolume(t(as.matrix(fitnesses[batch_nr == i, -"batch_nr"])), ref = ref_point))
# }



# set.seed(87456)
# library(counterfactuals)
# library(randomForest)
# data("PimaIndiansDiabetes", package = "mlbench")
# set.seed(5456465)
# rf_pima = randomForest::randomForest(diabetes ~ ., data = PimaIndiansDiabetes, ntree = 20L)
# pred_pima = iml::Predictor$new(rf_pima, data = PimaIndiansDiabetes, y = "diabetes", type = "prob")
# x_interest = PimaIndiansDiabetes[8L, ]
# y_hat_interest = pred_pima$predict(x_interest)
# y_hat_interest
# 
# moc_classif = MOCClassif$new(pred_pima, n_generations = 10L, fixed_features = c("pregnant", "age"))
# cfactuals = moc_classif$find_counterfactuals(x_interest, desired_class = "pos", desired_prob = c(0.75, 1))
# this_data = moc_classif$optimizer$archive$data[, c("batch_nr", "dist_target", "dist_x_interest", "nr_changed", "dist_train")]
# 
# ref_point = c(min(abs(pred_pima$predict(x_interest)[["pos"]] - c(0.75, 1))), 1, ncol(x_interest) - 1, 1)
# 
# for (i in 1:max(this_data$batch_nr)) {
#   print(ecr::computeHV(t(as.matrix(this_data[batch_nr == i, -"batch_nr"])), ref_point))
# }
# 
# 
# for (i in 1:max(this_data$batch_nr)) {
#   print(miesmuschel:::domhv(as.matrix(this_data[batch_nr == i, -"batch_nr"]), nadir = ref_point))
# }
# 
# for (i in 1:max(this_data$batch_nr)) {
#   print(emoa::dominated_hypervolume(t(as.matrix(this_data[batch_nr == i, -"batch_nr"])), ref = ref_point))
# }




# set.seed(5748554)
# library(randomForest)
# data("Boston", package  = "MASS")
# rf =  randomForest(medv ~ ., data = Boston)
# X = Boston[-which(names(Boston) == "medv")]
# mod = Predictor$new(rf, data = X)
# 
# # Then we explain the prediction of the first instance with the
# # Counterfactuals method
# x_interest = X[1L, ]
# target = 30
# 
# moc_regr = MOCRegr$new(mod, n_generations = 10L, use_conditional_mutator = TRUE)
# moc_regr$find_counterfactuals(x_interest, target)



