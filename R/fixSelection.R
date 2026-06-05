#' Probabilistic predictions with fixed-item panels based on a latent-variable model
#'
#' @description
#' \code{fixSelection()} performs probabilistic predictions for a fixed set
#' of selected items from a fitted latent-variable model. Currently, Latent Class (LC)
#' models fitted with \code{\link{fitLc}} and Multdimensional Item Response Theory
#' (MIRT) models fitted with \code{\link{fitIrt}} are supported. In contrast to
#' \code{\link{dedaptive}}, no adaptive selection is performed: a pre-specified set
#' of items (\code{givenVar}) is used.
#'
#' @details
#' The function computes the joint distribution of item responses conditional
#' on the selected items (if provided) and then applies score functions
#' (\code{funOfItems}) and thresholds (\code{thres}) to obtain predicted
#' distributions of scores and decisions. The arguments \code{funOfItems} and
#' \code{thres} must have the same length (one threshold per score function).
#'
#' @param model Model object fitted with \code{\link{fitIrt}} or
#'   \code{\link{fitLc}}.
#' @param predJointSub Optional object containing the predicted distribution of
#'   response patterns for the current person, as returned
#'   by \code{\link{predJointDistResp}}.
#' @param predJointSubCond Optional prediction of the joint distribution of
#'   item responses for the current person already conditional on
#'   \code{givenVar}. If supplied, it overrides \code{predJointSub} and no
#'   further conditioning is performed.
#' @param dataSub One-row data frame for the current person containing item
#'   responses and, if applicable, predictor variables considered in the latent regression
#'   of the MIRT or LC model.
#' @param funOfItems List of functions used to compute score(s) from the item
#'   responses (e.g., \code{list(sum)} for a sum score over all items, or several functions
#'   for multiple decisions).
#' @param thres Numeric vector of thresholds applied to the scores computed by
#'   \code{funOfItems} to define binary decisions (one threshold per score
#'   function). The length of \code{thres} must match the length of \code{funOfItems}.
#' @param givenVar Optional character vector with the names of items that are
#'   treated as "selected" (fixed) and whose observed values in \code{dataSub}
#'   are conditioned on. If \code{NULL}, predictions are based solely on the
#'   predictor variables without conditioning on specific items.
#' @param nSimLatent Number of latent variable or latent class draws used
#'   internally in \code{\link{predJointDistResp}} when simulation-based
#'   predictions are used.
#' @param nSimItem Number of response patterns simulated per latent draw used
#'   internally in \code{\link{predJointDistResp}} when simulation-based
#'   predictions are used.
#' @param seed Integer seed used to make the sequential selection procedure and
#'   simulations reproducible.
#' @param fullJoint Logical; for LC models, if \code{TRUE}, the full joint
#'   item-response distribution is computed exactly. If \code{FALSE}, the distribution
#'   is approximated by simulations. For MIRT models, \code{fullJoint = TRUE} is
#'   ignored and simulation-based predictions are used.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{pred}}{One-row data frame with predicted means
#'     (\code{predMean_}), probabilities \code{P(score >= thres)}
#'     (\code{prob_}), true score values (\code{trueMean_}), true decisions
#'     (\code{diag_}), number of selected items (\code{nItems}), selected item
#'     combination (\code{combItems}), total run time in seconds (\code{runTime})
#'     and run time per selected item (\code{runTimePerItem}).}
#'   \item{\code{distFun}}{Predicted distribution of the scores (computed with  \code{funOfItems})
#'   and decisions (based on  \code{thres}).}
#'   \item{\code{chosen}}{Character vector with the names of the selected items
#'     in the order in which they were chosen.}
#'   \item{\code{distItems}}{Joint distribution of the not chosen items at the
#'     end of the procedure.}
#'   \item{\code{distLatent}}{Posterior distribution of the latent variable or
#'     latent classes after the final adaptive update.}
#' }
#'
#' @import mirt
#' @export
fixSelection <- function(model,
                         predJointSub = NULL,
                         predJointSubCond = NULL,
                         dataSub,
                         funOfItems = list(sum),
                         thres,
                         givenVar = NULL,
                         nSimLatent = 1000,
                         nSimItem = 10,
                         seed = 131820,
                         fullJoint = FALSE
) {

  # (1) Preparation
  # Checks
  checkPredModel(model)

  if (!is.data.frame(dataSub) || nrow(dataSub) != 1L) {
    stop("'dataSub' must be a one-row data frame.")
  }

  if (!is.list(funOfItems) || length(funOfItems) == 0L) {
    stop("'funOfItems' must be a non-empty list of functions.")
  }

  if (length(thres) != length(funOfItems)) {
    stop("'thres' must have the same length as 'funOfItems'.")
  }

  if (!is.null(givenVar) && !all(givenVar %in% model$items)) {
    stop("All elements of 'givenVar' must be item names in 'model$items'.")
  }

  # Time stamp at the beginning
  timeStamp1 <- Sys.time()

  # Extract names of response variables from the model
  items <- model$items

  # Extract given values (if any) from dataSub
  if (!is.null(givenVar)) {
    givenVal <- as.numeric(dataSub[, givenVar, drop = FALSE])
    names(givenVal) <- givenVar
  } else {
    givenVal <- NULL
  }

  # (2) Compute joint distribution conditional on given variables

  if (is.null(predJointSubCond)) {

    if (is.null(predJointSub)) {
      # No pre-computed joint distribution: compute it from the IRT model
      predJointSub <- predJointDistResp(
        model = model,
        dataSub = dataSub,
        nSimLatent = nSimLatent,
        nSimItem = nSimItem,
        seed = seed,
        givenVal = givenVal,
        fullJoint = fullJoint
      )
      predJointDistSub <- predJointSub$jointDist
    } else {
      # Use provided joint distribution before conditioning on givenVar
      predJointDistSub <- predJointSub$jointDist

      if (!is.null(givenVar) && length(givenVar) > 0) {

        # Sequentially condition the joint distribution on all givenVar values
        for (i in seq_along(givenVal)) {
          predJointDistSub <- multiMultinomCondFromJoint(
            jointDist = predJointDistSub,
            varName = names(givenVal)[i],
            varValue = givenVal[i]
          )$cond
        }

        # Make sure the conditioned items have the correct observed values
        predJointDistSub[, givenVar] <- as.numeric(dataSub[, givenVar, drop = FALSE])

        # Reorder columns to have all responses and the frequency column
        predJointDistSub <- predJointDistSub[, c(items, "freq")]
      }
    }

  } else {
    # Use fully precomputed joint distribution conditional on givenVar
    predJointDistSub <- predJointSubCond$jointDist
  }

  # (3) Apply score functions and thresholds

  for (f in seq_along(funOfItems)) {
    # Scores (functions of item responses)
    predJointDistSub[[paste0("fun_", f)]] <-
      apply(predJointDistSub[, items, drop = FALSE],
            1, funOfItems[[f]])

    # Binary decisions based on thresholds
    predJointDistSub[[paste0("diag_", f)]] <-
      ifelse(predJointDistSub[[paste0("fun_", f)]] >= thres[f], 1, 0)
  }

  # (4) Compute predictions from the joint distribution

  # Predict distribution of scores: relative frequencies, means and
  # probabilities P(score >= threshold)
  out <- predFromJoint(predJointDistSub, thres)

  # True values and decisions based on all items
  for (f in seq_along(funOfItems)) {
    # True score computed from all responses in dataSub
    out$pred[[paste0("trueMean_", f)]] <-
      funOfItems[[f]](dataSub[, items])

    # True decision based on the true score and threshold
    out$pred[[paste0("diag_", f)]] <-
      ifelse(out$pred[[paste0("trueMean_", f)]] >= thres[f], 1, 0)
  }

  # Add number and names of selected items
  if (is.null(givenVar)){
    out$pred$nItems <- 0
  } else {
    out$pred$nItems <-length(givenVar)
  }

  if (is.null(givenVar)) {
    out$chosen <- ""
    out$pred$combItems <- ""
  } else {
    out$chosen <- givenVar
    out$pred$combItems <- paste(givenVar, collapse = ", ")
  }


  # Runtime information
  timeStamp2 <- Sys.time()
  out$pred$runTime <- difftime(timeStamp2, timeStamp1, units = "secs")[[1]]
  out$pred$runTimePerItem <-  out$pred$runTime

  # Add joint distribution of not chosen items and latent variables
  out$distItems <- predJointDistSub

  if (!is.null(predJointSubCond)) {
    out$distLatent <- predJointSubCond$postdistLatent
  } else {
    out$distLatent <- predJointSub$postdistLatent # prior if predJointSub is used
  }

  # Add the score functions and thresholds
  out$funOfItems <- funOfItems
  out$thres <- thres
  return(out)
}
