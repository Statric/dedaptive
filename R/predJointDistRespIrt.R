#' Probabilistic predictions of joint item responses from an Item Response Theory model
#'
#' @description
#' \code{predJointDistRespIrt()} performs probabilistic predictions of item
#' responses based on a Multidimensional Item Response Theory (MIRT) model fitted with \code{\link{fitIrt}}.
#' The method can incorporate predictors (via a latent regression, if specified) and
#' optionally already observed responses for a subset of items.
#'
#' The predictions are obtained with the following steps:
#' \enumerate{
#'   \item Predict the distribution of the latent variables (possibly
#'   conditional on predictors via the latent regression).
#'   \item If \code{givenVal} is supplied, update the distribution of the latent 
#'   variables conditional on the observed item responses.
#'   \item Draw samples of latent variables from this distribution.
#'   \item For each sampled latent variable, simulate item response patterns
#'   from the MIRT model, assuming local independence of items given the
#'   latent variables.
#'   \item Approximate the joint distribution of item responses
#'   based on the simulated response patterns.
#' }
#'
#' @param model A MIRT model fitted with \code{\link{fitIrt}}.
#' @param dataSub One-row data frame with the predictor variables of one
#' observation used in the latent regression (if specified in \code{model}).
#' @param nSimLatent Integer; number of simulated draws of the latent variables.
#' @param nSimItem Integer; number of simulated response patterns per latent
#' draw (for each sampled latent variable, \code{nSimItem} independent response
#' patterns are generated).
#' @param seed Integer seed for reproducibility of all simulations.
#' @param givenVal Optional named numeric vector of already observed item
#' responses for a subset of items (e.g., \code{c(phq1 = 2, phq2 = 1)}). The
#' names must match item names in \code{model$items}. If \code{NULL}, no
#' responses are conditioned on and the prior distribution of the latent
#' variables is predicted.
#' @param priorGrid Optional list representing an approximated distribution of
#' the latent variables from a previous step (typically the
#'   \code{postDistLatent} element from an earlier call to
#'   \code{predJointDistRespIrt}). If \code{NULL}, the prior multivariate
#'   distribution of the latent variables implied by the MIRT model (and latent
#'   regression, if specified) is used.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{postDistLatent}}{If \code{givenVal} is not \code{NULL}, a list
#'     with components \code{dist} (matrix containing grid points of the latent
#'     variables and their posterior probabilities) and \code{givenVal} (all
#'     item responses that have been conditioned on).}
#'   \item{\code{sim}}{Data frame containing all simulated response patterns
#'     generated from the MIRT model given the simulated latent variables.}
#'   \item{\code{jointDist}}{Data frame containing the approximated joint
#'     distribution of item responses. Each row corresponds to a distinct
#'     response pattern; the last column \code{freq} contains its relative
#'     frequency (approximated probability).}
#' }
#'
#' @import mirt
#' @export

predJointDistRespIrt <- function(model,
                                 dataSub,
                                 nSimLatent = 1000,
                                 nSimItem = 10,
                                 seed = 131820,
                                 givenVal = NULL,
                                 priorGrid = NULL) {

  # (1) Preparation
  # Checks

  checkPredModel(model)

  if (!is.data.frame(dataSub) || nrow(dataSub) != 1L) {
    stop("'dataSub' must be a one-row data frame.")
  }

  if (!is.numeric(nSimLatent) || length(nSimLatent) != 1L || nSimLatent < 1) {
    stop("'nSimLatent' must be a positive integer.")
  }

  if (!is.numeric(nSimItem) || length(nSimItem) != 1L || nSimItem < 1) {
    stop("'nSimItem' must be a positive integer.")
  }
  nSimLatent <- as.integer(nSimLatent)
  nSimItem <- as.integer(nSimItem)

  if (!is.null(givenVal)) {
    if (is.null(names(givenVal)) || !(all(names(givenVal) %in% model$items))) {
      stop("Names of 'givenVal' must match item names in 'model$items'.")
    }
  }

  # Extract information from model
  items <- model$items
  varLabels <- model$varLabels

  nFactors <- model$nFactors
  thetaGridPred <- model$thetaGridPred
  itemProbs <- model$itemProbs
  covFormula <- model$covFormula

  thetaCovPrior <- model$coef$thetaCovPrior
  regCoef <- model$coef$paramReg
  slopesItems <- model$coef$slopesItems
  intItems <- model$coef$intItems

  # Initialize output list
  outList <- list()

  # (2) Prior distribution of latent variables

  if (is.null(givenVal) || is.null(priorGrid)) {

    # prior mean of latent variables
    if (is.null(model$formula)) {
      # no latent regression: mean zero
      thetaMeanPrior <- rep(0, nFactors)
    } else {
      # latent regression case

      ## formula of latent regression model
      formReg <- covFormula

      ## design matrix and prior mean
      modelMat <- stats::model.matrix(formReg, dataSub)
      thetaMeanPrior <- as.numeric(modelMat %*% regCoef)
    }
  }

  # (3) Simulate latent variables
  ## (3a) Simulate from prior
  if (is.null(givenVal)) {

    if (length(thetaMeanPrior) <= 1) {
      set.seed(seed)
      thetaSim <- stats::rnorm(
        n = nSimLatent,
        mean = thetaMeanPrior,
        sd = sqrt(thetaCovPrior)
      )
    } else {
      set.seed(seed)
      thetaSim <- MASS::mvrnorm(
        n = nSimLatent,
        mu = thetaMeanPrior,
        Sigma = thetaCovPrior
      )
      # Define thetaSim as matrix also if only one simulation is performed
      if (nSimLatent == 1) {
        thetaSim <- matrix(thetaSim,
                           nrow = 1,
                           ncol = length(thetaMeanPrior))
      }
    }

  } else {

    ## (3b) Known responses: approximate posterior of latent variables on a grid
    # matrix with given response pattern (NA for unknown items)
    givenRespPattern <- matrix(NA, ncol = length(items), nrow = 1)
    colnames(givenRespPattern) <- items
    givenRespPattern[, names(givenVal)] <- as.numeric(givenVal)

    # Approximation of the prior distribution of latent variables (via grid)
    if (is.null(priorGrid)) {
      ## prediction grid created in fitIrt()
      thetaGrid <- thetaGridPred

      ## prior density without responses
      if (length(thetaMeanPrior) <= 1) {
        priorDens <- function(theta) {
          stats::dnorm(theta,
                       mean = thetaMeanPrior,
                       sd = sqrt(thetaCovPrior))
        }
      } else {
        priorDens <- function(theta) {
          mvtnorm::dmvnorm(theta,
                           mean = thetaMeanPrior,
                           sigma = thetaCovPrior)
        }
      }

      ## prior density on all grid points
      priorDistTheta <- apply(thetaGrid, 1, priorDens)

    } else {
      ## use approximation from previous step

      givenValPast <- priorGrid$givenVal
      priorGrid <- priorGrid$dist

      thetaGrid <- priorGrid[, -ncol(priorGrid), drop = FALSE]
      priorDistTheta <- priorGrid[,  ncol(priorGrid)]

      if (!isTRUE(all.equal(
        as.matrix(thetaGrid),
        as.matrix(thetaGridPred),
        check.attributes = FALSE
      ))) {
        stop("'priorGrid' must use the same theta grid as 'model$thetaGridPred'.")
      }
    }

    # Likelihood of given responses conditional on thetaGrid

    ## initialize likelihood
    likelihood <- rep(1, nrow(thetaGrid))

    for (j in seq_len(ncol(givenRespPattern))) {
      ## response of item j 
      response <- givenRespPattern[1, j]

      ## labels for item j
      varLabels_j <- varLabels[[j]]

      if (!is.na(response)) {
        ## find closest category
        respCol <- which(
          abs(as.numeric(varLabels_j) - response) <=
            min(abs(as.numeric(varLabels_j) - response))
        )
        respCol <- respCol[1]
        
        ## item category probabilities conditional on latent variable
        probs <- itemProbs[[j]][, respCol]

        ## update likelihood (conditional independence)
        likelihood <- likelihood * probs
      }
    }

    # Posterior over the grid
    postDistTheta <- likelihood * priorDistTheta
    postDistTheta <- postDistTheta / sum(postDistTheta)

    # sample latent variables from posterior
    set.seed(seed)
    simIds <- sample(seq_len(nrow(thetaGrid)),
                     size = nSimLatent,
                     replace = TRUE,
                     prob = postDistTheta)
    thetaSim <- thetaGrid[simIds, , drop = FALSE]

    # save approximation of posterior in output
    postDistTheta <- cbind(thetaGrid, postDistTheta)
    if (is.null(priorGrid)) {
      outList$postDistLatent <- list(dist = postDistTheta,
                                     givenVal = givenVal)
    } else {
      outList$postDistLatent <- list(dist = postDistTheta,
                                     givenVal = c(givenValPast, givenVal))
    }
  }

  # (4) Simulate response values

  # matrix with repeated theta values
  if (is.vector(thetaSim)) {
    thetaSimRep <- matrix(rep(thetaSim, nSimItem), ncol = 1L)
  } else {
    thetaSimRep <- matrix(
      rep(t(thetaSim), nSimItem),
      ncol = ncol(thetaSim),
      byrow = TRUE
    )
  }

  # set the seed
  set.seed(seed + 1)

  # simulate response patterns for given theta values
  respSim <- mirt::simdata(slopesItems, intItems,
                           Theta = thetaSimRep,
                           itemtype = "graded")
  colnames(respSim) <- items

  # Consider the known values (if any)
  if (!is.null(outList$postDistLatent$givenVal)) {
    for (i in seq_along(outList$postDistLatent$givenVal)) {
      respSim[, names(outList$postDistLatent$givenVal)[i]] <-
        as.numeric(outList$postDistLatent$givenVal[i])
    }
  }

  # (5) Approximate joint distribution of response combinations
  respComb <- as.data.frame(respSim)
  freqTable <- plyr::count(respComb)
  freqTable$freq <- freqTable$freq / sum(freqTable$freq)

  colnames(freqTable) <- c(items, "freq")
  
  freqTable[, items] <- lapply(
    freqTable[, items, drop = FALSE],
    facToNumeric
  )
  # Save results
  outList$sim <- respComb
  outList$jointDist <- freqTable

  return(outList)
}
