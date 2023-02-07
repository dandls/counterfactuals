#' @description 
#' 
#' RandomSearch randomly samples a population of candidates and returns non-dominated candidates w.r.t to the objectives
#' of MOC (Dandl et. al 2020) as counterfactuals. RandomSearch is equivalent to MOC with zero generations and the `random`
#' initialization strategy.
#' 
#' 
#' The four objectives of MOC (Dandl et. al 2020) to are: 
#' \enumerate{
#'    \item {Distance to `desired_prob` (classification tasks) or `desired_prob` (regression tasks).}
#'    \item {Dissimilarity to `x_interest` measured by Gower's dissimilarity measure (Gower 1971).}
#'    \item {Number of feature changes.}
#'    \item {(Weighted) sum of dissimilarities to the `k` nearest data points in `predictor$data$X`.}
#' }  
#' 
#' 
#' @details 
#' 
#' RandomSearch is typically used as a baseline in benchmark comparisons with MOC. 
#' The total number of samples drawn is `mu` * `n_generations`. Using separate parameters `mu` and `n_generations`
#' is only required to make certain statistics comparable with MOC (e.g. the evolution of the dominated hypervolume).
#' 
#' 
#' @references 
#' Dandl, S., Molnar, C., Binder, M., and Bischl, B. (2020). 
#' "Multi-Objective Counterfactual Explanations". In: Parallel Problem 
#' Solving from Nature – PPSN XVI, edited by Thomas Bäck, Mike Preuss, 
#' André Deutz, Hao Wang, Carola Doerr, Michael Emmerich, and Heike Trautmann, 448–469, 
#' Cham, Springer International Publishing, \doi{10.1007/978-3-030-58112-1_31}.
#' 
#' Deb, K., Pratap, A., Agarwal, S., & Meyarivan, T. A. M. T. (2002). 
#' "A fast and elitist multiobjective genetic algorithm: NSGA-II". 
#' IEEE transactions on evolutionary computation, 6(2), 182-197.
#' 
#' Goldstein, A., Kapelner, A., Bleich, J., and Pitkin, E. (2015). 
#' "Peeking Inside the Black Box: Visualizing 
#' Statistical Learning with Plots of Individual Conditional Expectation". 
#' Journal of Computational and Graphical Statistics 24 (1): 44–65. 
#' \doi{10.1080/10618600.2014.907095}.
#' 
#' Gower, J. C. (1971). A general coefficient of similarity and some of its properties. Biometrics, 27, 623–637.
#' 
#' Li, Rui, L., Emmerich, M. T. M., Eggermont, J. Bäck, T., Schütz, M., Dijkstra, J., Reiber, J. H. C. (2013). 
#' "Mixed Integer Evolution Strategies for Parameter Optimization." 
#' Evolutionary Computation 21 (1): 29–64. \doi{10.1162/EVCO_a_00059}.
