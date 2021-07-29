#' @description 
#' `MOC` (Dandl et. al 2020) solves a multi-objective optimization problem to find counterfactuals. The four objectives
#' to minimize are:
#' \enumerate{
#'    \item {Distance between `x_interest` and `desired_prob`}
#'    \item {Distance between `x_interest` and a candidate}
#'    \item {Number of feature changes}
#'    \item {(Weighted) average distance between a candidate and its `k` nearest observed data points}
#' }  
#' 
#' For optimization it uses the NSGA II algorithm (Deb et. al 2002) with mixed integer evolutionary 
#' strategies by Li et al. (2013). 
#' 
#' @details 
#' 
#' Several population initialization strategies are available:
#' \enumerate{
#'    \item {`random`: Sample from numerical feature ranges and discrete feature values from `predictor$data$X`. 
#'    Some features values are randomly reset to the values of `x_interest`.}
#'    \item {`icecurve`: Sample from numerical feature ranges and discrete feature values from `predictor$data$X`. 
#'    The higher the ICE curve variance of a feature, the lower the probability that
#'    values of this feature are reset to the values of `x_interest`.}
#'    \item {`sd`: Sample from numerical feature ranges that are limited by the feature standard deviations extracted
#'    from `predictor$data$X`. For non-numerical features, the `random` strategy is used. 
#'    Some features values are randomly reset to the values of `x_interest`.}
#'    \item {`traindata`: Initializes the first population using observations from `predictor$data$X` that are nondominated. 
#'    Some features values are randomly reset to the values of `x_interest`. If not enough nondominated observations are found, 
#'    remaining individuals are created using the `random` strategy.}
#' }  
#' 
#' The R package `miesmuschel` implements the mixed integer evolutionary strategies.\cr
#' To compute dissimilarities, the function uses Gower's dissimilarity measure (Gower, 1990), 
#' which is implemented in the \link[StatMatch]{gower.dist}. 
#' 
#' @references 
#'
#' Dandl, Susanne, Christoph Molnar, Martin Binder, and Bernd Bischl. 2020. “Multi-Objective Counterfactual Explanations.” 
#' In Parallel Problem Solving from Nature – PPSN XVI, edited by Thomas Bäck, Mike Preuss, André Deutz, Hao Wang, Carola Doerr, 
#' Michael Emmerich, and Heike Trautmann, 448–69. Cham: Springer International Publishing.
#' 
#' Deb, K., Pratap, A., Agarwal, S., & Meyarivan, T. A. M. T. (2002). A fast and elitist multiobjective genetic algorithm: NSGA-II. 
#' IEEE transactions on evolutionary computation, 6(2), 182-197.
#' 
#' Gower, J. C. (1971), "A general coefficient of similarity and some of its properties". Biometrics, 27, 623–637.
#' 
#' R. Li et al., "Mixed Integer Evolution Strategies for Parameter Optimization," in Evolutionary Computation, vol. 21, no. 1, 
#' pp. 29-64, March 2013, doi: 10.1162/EVCO_a_00059.
