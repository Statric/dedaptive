#' dedaptive: Decision-oriented adaptive testing based on latent variable models
#'
#' This package implements DEcision-oriented aDAPTIVE (dedaptive) testing.
#' Items are selected sequentially to minimize the expected costs of
#' misclassification plus the costs of administering items. Predictions are
#' based on Multidimensional Item Response Theory (MIRT) models for graded
#' (ordinal) responses fitted with \code{\link[mirt]{mirt}}, or on Latent
#' Class (LC) models fitted with \code{\link[poLCA]{poLCA}}.
#'
#' @import mirt
#' @import poLCA
#' @importFrom graphics layout par abline text rect segments axis
#' @keywords internal
"_PACKAGE"
