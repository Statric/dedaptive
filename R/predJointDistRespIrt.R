#' Probabilistic predictions of joint item responses from an IRT model
#'
#' @description
#' \code{predJointDistRespIrt()} performs probabilistic predictions of item
#' responses based on an IRT model fitted with \code{\link{fitIrt}}. The method
#' can incorporate predictors (via a latent regression, if specified) and
#' optionally already observed responses for a subset of items.
#'
#' The predictions are obtained in four steps (see Wyss et al. (2025) for
#' further details):
#' \enumerate{
#'   \item Predict the distribution of the latent variables (possibly
#'   conditional on predictors via the latent regression).
#'   \item Draw samples of latent variables from this distribution.
#'   \item For each sampled latent variable, simulate item response patterns
#'   from the IRT model, assuming conditional independence of items given the
#'   latent variables.
#'   \item Approximate the (conditional) joint distribution of item responses
#'   based on the simulated response patterns.
#' }
#'
#' @param model An IRT model fitted with \code{\link{fitIrt}}. This object
#'   contains the fitted \code{mirt} model and meta-data (item names, response
#'   labels, latent regression formula).
#' @param dataSub One-row data frame with the predictor variables for the
#'   current person used in the latent regression (if specified in \code{model}).
#' @param nSimTheta Integer; number of simulated draws of the latent variables.
#' @param nSimItem Integer; number of simulated response patterns per latent
#'   draw (for each sampled latent variable, \code{nSimItem} independent
#'   response patterns are generated).
#' @param seed Integer seed for reproducibility of all simulations.
#' @param givenVal Optional named numeric vector of already observed item
#'   responses for a subset of items (e.g., \code{c(PHQ1 = 2, PHQ3 = 1)}). Names
#'   must match item names in \code{model$respName}. If \code{NULL}, no
#'   responses are conditioned on and predictions are based on the prior
#'   distribution of the latent variables.
#' @param priorGrid Optional list representing an approximated distribution of
#'   the latent variables from a previous step (typically the
#'   \code{postDistTheta} element from an earlier call to
#'   \code{predJointDistRespIrt}). If \code{NULL}, the prior multivariate
#'   distribution of the latent variables implied by the IRT model (and latent
#'   regression, if specified) is used.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{postDistTheta}}{If \code{givenVal} is not \code{NULL}, a list
#'     with components \code{dist} (matrix with grid points of the latent
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
#' @references
#' Wyss, P., Steiner, D., Lopes, R., M., Sipka, Berger, T., & Krause, A. (2025).
#' Decision-Oriented Adaptive Testing for Efficient Screening Across Mental Disorders.
#' Manuscript in preparation.
predJointDistRespIrt <- function(model, dataSub,
                                 nSimTheta = 500, nSimItem  = 2, seed = 131820,
                                 givenVal  = NULL, priorGrid = NULL) {

  # extract information from model
  respNames <- model$respName
  formula   <- model$formula
  varLabels <- model$varLabels
  nResp     <- length(respNames)

  # Initialize output list
  outList <- list()

  # (1) Prior distribution of latent variables

  if (is.null(givenVal) | is.null(priorGrid)) {

    # covariance matrix of prior distribution of the latent variables
    thetaCovPrior <- mirt::coef(model$fit, simplify = TRUE)$cov

    # prior mean of latent variables
    if (is.null(model$formula)) {
      # no latent regression: mean zero
      dataReg        <- NULL
      thetaMeanPrior <- rep(0, length(mirt::extract.mirt(model$fit, "factorNames")))
    } else {
      # latent regression case

      ## regression weights
      regCoef <- mirt::coef(model$fit, simplify = TRUE)$lr.betas

      ## formula of latent regression model
      formReg <- stats::as.formula(paste0("~", model$formula))

      ## predictor names
      varReg  <- all.vars(formReg)

      ## data for regression
      dataReg <- dataSub[, varReg, drop = FALSE]

      ## design matrix and prior mean
      modelMat       <- stats::model.matrix(formReg, dataSub)
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
    givenRespPattern <- matrix(NA, ncol = length(respNames), nrow = 1)
    colnames(givenRespPattern) <- respNames
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
    items <- model$fit@ParObjects$pars[1:model$fit@Data$nitems]

    ## probabilities of item values for all theta values in the grid
    itemProbs <- lapply(items, function(item) mirt::probtrace(item, thetaGrid))

    ## initialize likelihood
    likelihood <- rep(1, nrow(thetaGrid))

    for (j in 1:length(givenRespPattern)) {
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
    thetaSim <- thetaGrid[simIds, ]
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
  if (is.null(ncol(thetaSim))) {
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
  slopesItems <- paramItems[, grepl("a", colnames(paramItems))]
  intItems    <- paramItems[, grepl("d", colnames(paramItems))]

  # set the seed
  set.seed(seed + 1)

  # simulate response patterns for given theta values
  respSim <- mirt::simdata(slopesItems, intItems,
                           Theta    = thetaSimRep,
                           itemtype = "graded")
  colnames(respSim) <- respNames

  # Consider the known values (if any)
  for (i in 1:length(outList$postDistTheta$givenVal)) {
    respSim[, names(outList$postDistTheta$givenVal)[i]] <-
      as.numeric(outList$postDistTheta$givenVal[i])
  }

  # (4) Approximate joint distribution of response combinations

  respComb  <- as.data.frame(respSim)
  freqTable <- plyr::count(respComb)
  freqTable$freq <- freqTable$freq / sum(freqTable$freq)

  colnames(freqTable) <- c(respNames, "freq")

  # Make item values numeric
  facToNumeric <- function(x) as.numeric(as.character(x))
  freqTable[, colnames(freqTable) != "freq"] <-
    apply(freqTable[, colnames(freqTable) != "freq"], 2, facToNumeric)

  # Save results
  outList$sim       <- respComb
  outList$jointDist <- freqTable

  return(outList)
}
