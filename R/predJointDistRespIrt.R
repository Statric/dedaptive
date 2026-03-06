#' Probabilistic predictions of joint item responses from an IRT model
#'
#' @description
#' \code{predJointDistRespIrt()} performs probabilistic predictions of item
#' responses based on a multidimensional Item Response Theory (IRT) model
#' fitted with \code{\link{fitIrt}}. The method
#' can incorporate predictors (via a latent regression, if specified) and
#' optionally already observed responses for a subset of items.
#'
#' The predictions are obtained in four steps (see Wyss et al. (2026) for
#' further details):
#' \enumerate{
#'   \item Predict the distribution of the latent variables (possibly
#'   conditional on predictors via the latent regression).
#'   \item Draw samples of latent variables from this distribution.
#'   \item For each sampled latent variable, simulate item response patterns
#'   from the IRT model, assuming conditional independence of items given the
#'   latent variables.
#'   \item Approximate the joint distribution of item responses
#'   based on the simulated response patterns.
#' }
#'
#' @param model A multidimensional IRT model fitted with \code{\link{fitIrt}}.
#' This object contains the fitted \code{\link[mirt]{mirt}} model and meta-data (item names, response
#' labels, latent regression formula).
#' @param dataSub One-row data frame with the predictor variables of one
#' observation used in the latent regression (if specified in \code{model}).
#' @param nSimTheta Integer; number of simulated draws of the latent variables.
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
#'   \code{postDistTheta} element from an earlier call to
#'   \code{predJointDistRespIrt}). If \code{NULL}, the prior multivariate
#'   distribution of the latent variables implied by the IRT model (and latent
#'   regression, if specified) is used.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{postDistTheta}}{If \code{givenVal} is not \code{NULL}, a list
#'     with components \code{dist} (matrix containing grid points of the latent
#'     variables and their posterior probabilities) and \code{givenVal} (all
#'     item responses that have been conditioned on so far).}
#'   \item{\code{sim}}{Data frame containing all simulated response patterns
#'     generated from the IRT model given the simulated latent variables.}
#'   \item{\code{jointDist}}{Data frame containing the approximated joint
#'     distribution of item responses. Each row corresponds to a distinct
#'     response pattern; the last column \code{freq} contains its relative
#'     frequency (probability).}
#' }
#'
#' @import mirt
#' @export
#'
#' @references Follow
predJointDistRespIrt <- function(model,
                                 dataSub,
                                 nSimTheta = 1000,
                                 nSimItem = 10,
                                 seed = 131820,
                                 givenVal = NULL,
                                 priorGrid = NULL) {

  # (1) Preparation
  # Checks
  if (!(is.list(model) || is.null(model$fit) || is.null(model$items))){
    stop("'model' must be an object returned by fitIrt().")
  }

  if (!(is.data.frame(dataSub) || nrow(dataSub) != 1L)) {
    stop("'dataSub' must be a one-row data frame.")
  }

  if (!(is.numeric(nSimTheta) || length(nSimTheta) != 1L || nSimTheta < 1)) {
    stop("'nSimTheta' must be a positive integer.")
  }

  if (!(is.numeric(nSimItem) || length(nSimItem) != 1L || nSimItem < 1)) {
    stop("'nSimItem' must be a positive integer.")
  }

  if (!is.null(givenVal)) {
    if (is.null(names(givenVal)) || !(all(names(givenVal) %in% model$items))) {
      stop("Names of 'givenVal' must match item names in 'model$items'.")
    }
  }

  # extract information from model
  items <- model$items
  formula   <- model$formula
  varLabels <- model$varLabels
  nResp     <- length(items)

  # Initialize output list
  outList <- list()

  # (1) Prior distribution of latent variables

  if (is.null(givenVal) || is.null(priorGrid)) {

    # covariance matrix of prior distribution of the latent variables
    thetaCovPrior <- mirt::coef(model$fit, simplify = TRUE)$cov

    # prior mean of latent variables
    if (is.null(model$formula)) {
      # no latent regression: mean zero
      thetaMeanPrior <- rep(0, length(mirt::extract.mirt(model$fit, "factorNames")))
    } else {
      # latent regression case

      ## regression weights
      regCoef <- mirt::coef(model$fit, simplify = TRUE)$lr.betas

      ## formula of latent regression model
      if (inherits(model$formula, "formula")) {
        formReg <- model$formula
      } else {
        formReg <- stats::as.formula(
          if (grepl("~", model$formula, fixed = TRUE)) model$formula else paste0("~", model$formula)
        )
      }

      ## design matrix and prior mean
      modelMat <- stats::model.matrix(formReg, dataSub)
      thetaMeanPrior <- as.numeric(modelMat %*% regCoef)
    }
  }

  # (2) Simulate latent variables
  ## (2a) Simulate from prior
  if (is.null(givenVal)) {

    if (length(thetaMeanPrior) <= 1) {
      set.seed(seed)
      thetaSim <- stats::rnorm(
        n    = nSimTheta,
        mean = thetaMeanPrior,
        sd   = sqrt(thetaCovPrior)
      )
    } else {
      set.seed(seed)
      thetaSim <- MASS::mvrnorm(
        n   = nSimTheta,
        mu  = thetaMeanPrior,
        Sigma = thetaCovPrior
      )
      # Define thetaSim as matrix also if only one simulation is performed
      if (nSimTheta == 1) {
        thetaSim <- matrix(thetaSim,
                           nrow = 1,
                           ncol = length(thetaMeanPrior))
      }
    }

  } else {

    ## (2b) Known responses: approximate posterior of latent variables on a grid
    # matrix with given response pattern (NA for unknown items)
    givenRespPattern <- matrix(NA, ncol = length(items), nrow = 1)
    colnames(givenRespPattern) <- items
    givenRespPattern[, names(givenVal)] <- as.numeric(givenVal)

    # Approximation of the prior distribution of latent variables (via grid)
    if (is.null(priorGrid)) {
      ## grid of theta values used in the fitted model
      thetaGrid <- model$fit@Model$Theta

      ## prior density without responses
      if (length(thetaMeanPrior) <= 1) {
        priorDens <- function(theta) {
          stats::dnorm(theta,
                       mean = thetaMeanPrior,
                       sd   = sqrt(thetaCovPrior))
        }
      } else {
        priorDens <- function(theta) {
          mvtnorm::dmvnorm(theta,
                           mean  = thetaMeanPrior,
                           sigma = thetaCovPrior)
        }
      }

      ## prior density on all grid points
      priorDistTheta <- apply(thetaGrid, 1, priorDens)

    } else {
      ## use approximation from previous step

      givenValPast <- priorGrid$givenVal
      priorGrid    <- priorGrid$dist

      thetaGrid      <- priorGrid[, -ncol(priorGrid), drop = FALSE]
      priorDistTheta <- priorGrid[,  ncol(priorGrid)]
    }

    # Likelihood of given responses conditional on thetaGrid

    ## item parameter objects
    itemsMirt <- model$fit@ParObjects$pars[1:model$fit@Data$nitems]

    ## probabilities of item values for all theta values in the grid
    itemProbs <- lapply(itemsMirt, function(item) mirt::probtrace(item, thetaGrid))

    ## initialize likelihood
    likelihood <- rep(1, nrow(thetaGrid))

    for (j in seq_along(givenRespPattern)) {
      ## response of item j (in column-major order; same as original)
      response <- givenRespPattern[j]

      ## labels for item j
      varLabels_j <- model$varLabels[[j]]

      if (!is.na(response)) {
        ## find closest category
        respCol <- which(
          abs(as.numeric(varLabels_j) - response) <=
            min(abs(as.numeric(varLabels_j) - response))
        )
        respCol <- respCol[1]

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
    simIds   <- sample(1:nrow(thetaGrid),
                       size    = nSimTheta,
                       replace = TRUE,
                       prob    = postDistTheta)
    thetaSim <- thetaGrid[simIds, , drop = FALSE]
    # thetaSim as matrix if only one value was simulated
    if (nSimTheta == 1) {
      thetaSim <- matrix(thetaSim,
                         nrow = 1,
                         ncol = ncol(thetaGrid))
    }

    # save approximation of posterior in output
    postDistTheta <- cbind(thetaGrid, postDistTheta)
    if (is.null(priorGrid)) {
      outList$postDistTheta <- list(dist = postDistTheta,
                                    givenVal = givenVal)
    } else {
      outList$postDistTheta <- list(dist     = postDistTheta,
                                    givenVal = c(givenValPast, givenVal))
    }
  }

  # (3) Simulate response values

  # matrix with repeated theta values
  if (is.vector(thetaSim)) {
    thetaSimRep <- rep(thetaSim, nSimItem)
  } else {
    thetaSimRep <- matrix(
      rep(t(thetaSim), nSimItem),
      ncol    = ncol(thetaSim),
      byrow   = TRUE
    )
  }

  # extract loadings (slopes) and intercepts per item
  paramItems  <- mirt::coef(model$fit, simplify = TRUE)$items
  slopesItems <- paramItems[, grepl("a", colnames(paramItems)), drop = FALSE]
  intItems    <- paramItems[, grepl("d", colnames(paramItems)), drop = FALSE]

  # set the seed
  set.seed(seed + 1)

  # simulate response patterns for given theta values
  respSim <- mirt::simdata(slopesItems, intItems,
                           Theta    = thetaSimRep,
                           itemtype = "graded")
  colnames(respSim) <- items

  # Consider the known values (if any)
  if (!is.null(outList$postDistTheta$givenVal)) {
    for (i in seq_along(outList$postDistTheta$givenVal)) {
      respSim[, names(outList$postDistTheta$givenVal)[i]] <-
        as.numeric(outList$postDistTheta$givenVal[i])
    }
  }

  # (4) Approximate joint distribution of response combinations

  respComb  <- as.data.frame(respSim)
  freqTable <- plyr::count(respComb)
  freqTable$freq <- freqTable$freq / sum(freqTable$freq)

  colnames(freqTable) <- c(items, "freq")

  # Make item values numeric
  facToNumeric <- function(x) as.numeric(as.character(x))
  freqTable[, colnames(freqTable) != "freq"] <-
    lapply(freqTable[, colnames(freqTable) != "freq", drop = FALSE], facToNumeric)

  # Save results
  outList$sim       <- respComb
  outList$jointDist <- freqTable

  return(outList)
}
