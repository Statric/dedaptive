#' Probabilistic predictions of joint item responses
#'
#' @description
#' \code{predJointDistResp()} is a generic wrapper for probabilistic prediction
#' of joint item-response distributions. It uses model-specific prediction functions
#' based on the model type (\code{model$modelType}). Currently, Latent Class (LC)
#' and Multidimensional Item Response Theory (MIRT) model are supported.
#'
#' If \code{model$modelType == "lc"} (LC model), the function calls
#' \code{\link{predJointDistRespLc}}. In this case, the joint response
#' distribution can either be computed exactly by setting \code{fullJoint = TRUE}
#' or approximated by simulation by setting \code{fullJoint = FALSE}.
#'
#' If \code{model$modelType == "irt"} (MIRT model), the function calls
#' \code{\link{predJointDistRespIrt}}. For MIRT models, \code{fullJoint = TRUE}
#' is ignored.
#'
#' Simulation-based predictions are obtained with the following steps:
#' \enumerate{
#'   \item Predict the distribution of the latent variable(s) (possibly
#'   conditional on predictors via the latent regression).
#'   \item If \code{givenVal} is supplied, update the distribution of the latent
#'   variable(s) conditional on the observed item responses.
#'   \item Draw samples of latent variables from this distribution.
#'   \item For each sampled latent variable, simulate item response patterns
#'   from the LC respectively MIRT model, assuming local independence of items given the
#'   latent variables.
#'   \item Approximate the joint distribution of item responses
#'   based on the simulated response patterns.
#' }
#'
#' @param model model fitted with \code{\link{fitLc}} or \code{\link{fitIrt}}.
#' @param dataSub One-row data frame with the predictor variables of one
#' observation used in the latent regression (if specified in \code{model}).
#' @param nSimLatent Integer; number of simulated latent variable(s) draws.
#' @param nSimItem Integer; number of simulated response patterns per latent
#' variable draw (for each sampled latent variables , \code{nSimItem} independent
#' response patterns are generated).
#' @param seed Integer seed for reproducibility of all simulations.
#' @param givenVal Optional named numeric vector of already observed item
#' responses for a subset of items (e.g., \code{c(phq1 = 2, phq2 = 1)}). The
#' names must match item names in \code{model$items}. If \code{NULL}, no
#' responses are conditioned on and the prior latent class probabilities are predicted.
#' @param priorGrid Optional list representing an approximated distribution of
#' latent variables from a previous step (typically the
#'   \code{postDistLatent} element from an earlier call to \code{predJointDistResp}).
#'   If \code{NULL}, the prior latent variable distribution implied by the LC or
#'   MIRT model (and latent regression, if specified) is used.
#' @param fullJoint Logical; for LC models, if \code{TRUE}, the full joint
#'   item-response distribution is computed exactly. If \code{FALSE}, the
#'   distribution is approximated by simulation. For MIRT models,
#'   \code{fullJoint = TRUE} is ignored and simulation-based prediction is used.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{postDistLatent}}{If \code{givenVal} is not \code{NULL}, a list
#'     with components \code{dist} (matrix containing posterior probabilities of the
#'     latent classes or grid point of the latent variables) and \code{givenVal} (all
#'     item responses that have been conditioned on).}
#'   \item{\code{sim}}{Data frame containing all simulated response patterns
#'     generated from the LC model given the simulated latent variables.}
#'   \item{\code{jointDist}}{Data frame containing the approximated joint
#'     distribution of item responses. Each row corresponds to a distinct
#'     response pattern; the last column \code{freq} contains its relative
#'     frequency (approximated probability).}
#' }
#'
#' @export
#'
predJointDistResp <- function(model,
                              dataSub,
                              nSimLatent = 1000,
                              nSimItem = 10,
                              seed = 131820,
                              givenVal = NULL,
                              priorGrid = NULL,
                              fullJoint = FALSE) {

  # (1) Checks

  if (!is.list(model)) {
    stop("'model' must be a model object returned by fitLc() or fitIrt().")
  }

  if (is.null(model$modelType)) {
    stop(
      "'model$modelType' is missing. ",
      "Please fit the model with fitLc() or fitIrt()."
    )
  }

  if (!is.character(model$modelType) || length(model$modelType) != 1L ||
      !(model$modelType %in% c("lc", "irt"))) {
    stop("'model$modelType' must be 'irt' or 'lc'.")
  }

  if (!is.logical(fullJoint) || length(fullJoint) != 1L || is.na(fullJoint)) {
    stop("'fullJoint' must be TRUE or FALSE.")
  }

  modelType <- model$modelType

  # (2) Prediction
  ## (2a) With latent class (LC) model
  if (modelType == "lc") {

    return(
      predJointDistRespLc(
        model = model,
        dataSub = dataSub,
        nSimLatent = nSimLatent,
        nSimItem = nSimItem,
        seed = seed,
        givenVal = givenVal,
        priorGrid = priorGrid,
        fullJoint = fullJoint
      )
    )
  }

  ## (2b) With item response theory (IRT) model

  if (modelType == "irt") {

    if (isTRUE(fullJoint)) {
      warning(
        "'fullJoint = TRUE' is ignored for IRT models. ",
        "Simulation-based predictions are performed.",
        call. = FALSE
      )
    }

    return(
      predJointDistRespIrt(
        model = model,
        dataSub = dataSub,
        nSimLatent = nSimLatent,
        nSimItem = nSimItem,
        seed = seed,
        givenVal = givenVal,
        priorGrid = priorGrid
      )
    )
  }
}
