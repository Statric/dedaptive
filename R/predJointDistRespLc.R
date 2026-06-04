#' Probabilistic predictions of joint item responses from a latent class model
#'
#' @description
#' \code{predJointDistRespLc()} performs probabilistic predictions of item responses
#' based on a Latent Class (LC) model fitted with \code{\link{fitLc}}.
#' The method can incorporate predictors (via a latent class regression, if specified) and
#' optionally already observed responses for a subset of items.
#'
#' The function either computes the full joint distribution of item responses or
#' approximates it via simulations. Simulation-based predictions are obtained
#' with the following steps:
#' \enumerate{
#'   \item Predict latent class-membership probabilities (possibly conditional on
#'   predictors via latent class regression).
#'   \item If \code{givenVal} is supplied, update the latent class probabilities
#'   conditional on the observed item responses.
#'   \item Draw samples of latent classes given these probabilities.
#'   \item For each sampled latent class, simulate item response patterns
#'   from the LC model, assuming local independence of items given the
#'   latent classes
#'   \item Approximate the joint distribution of item responses by aggregating
#'   the simulated response patterns.
#' }
#'
#' @param model A LC model fitted with \code{\link{fitLc}}.
#' @param dataSub One-row data frame with the predictor variables of one
#' observation used in the latent regression (if specified in \code{model}).
#' @param nSimLatent Integer; number of simulated latent class draws.
#' @param nSimItem Integer; number of simulated response patterns per latent
#' class draw (for each sampled latent class, \code{nSimItem} independent response
#' patterns are generated).
#' @param seed Integer seed for reproducibility of all simulations.
#' @param givenVal Optional named numeric vector of already observed item
#' responses for a subset of items (e.g., \code{c(phq1 = 2, phq2 = 1)}). The
#' names must match item names in \code{model$items}. If \code{NULL}, no
#' responses are conditioned on and the prior latent class probabilities are predicted.
#' @param priorGrid Optional list representing an approximated latent class
#' distribution from a previous step (typically the
#'   \code{postDistLatent} element from an earlier call to
#'   \code{predJointDistRespLc}). If \code{NULL}, prior latent class probabilities
#'   implied by the LC model (and latent regression, if specified) is used.
#' @param fullJoint Logical; if \code{TRUE}, the full joint item-response
#'   distribution is computed exactly by enumerating response patterns. If
#'   \code{FALSE}, the distribution is approximated by simulation.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{postDistLatent}}{If \code{givenVal} is not \code{NULL}, a list
#'     with components \code{dist} (matrix containing the latent classes and 
#'     their posterior probabilities) and \code{givenVal} (all
#'     item responses that have been conditioned on).}
#'   \item{\code{sim}}{Data frame containing all simulated response patterns
#'     generated from the LC model given the simulated latent variables.}
#'   \item{\code{jointDist}}{Data frame containing the approximated joint
#'     distribution of item responses. Each row corresponds to a distinct
#'     response pattern; the last column \code{freq} contains its relative
#'     frequency (approximated probability).}
#' }
#'
#' @import poLCA
#' @export
#'
predJointDistRespLc <- function(model,
                                dataSub,
                                nSimLatent = 1000,
                                nSimItem = 10,
                                seed = 131820,
                                givenVal = NULL,
                                priorGrid = NULL,
                                fullJoint = FALSE) {

  # (1) Preparation
  # Checks

  checkPredModel(model)

  if (!is.data.frame(dataSub) || nrow(dataSub) != 1L) {
    stop("'dataSub' must be a one-row data frame.")
  }

  if (!is.logical(fullJoint) || length(fullJoint) != 1L || is.na(fullJoint)) {
    stop("'fullJoint' must be TRUE or FALSE.")
  }

  if (!isTRUE(fullJoint)) {

    if (!is.numeric(nSimLatent) || length(nSimLatent) != 1L ||
        is.na(nSimLatent) || nSimLatent < 1 ||
        nSimLatent != as.integer(nSimLatent)) {
      stop("'nSimLatent' must be a positive integer.")
    }

    if (!is.numeric(nSimItem) || length(nSimItem) != 1L ||
        is.na(nSimItem) || nSimItem < 1 ||
        nSimItem != as.integer(nSimItem)) {
      stop("'nSimItem' must be a positive integer.")
    }

    nSimLatent <- as.integer(nSimLatent)
    nSimItem <- as.integer(nSimItem)
  }

  if (!is.null(givenVal)) {
    if (is.null(names(givenVal)) ||
        !all(names(givenVal) %in% model$items)) {
      stop("Names of 'givenVal' must match item names in 'model$items'.")
    }
  }

  # Extract information from model
  items <- model$items
  varLabels <- model$varLabels
  nClasses <- model$nClasses
  itemProbs <- model$itemProbs

  # Initialize output list
  outList <- list()

  # (2) Prior latent class distribution

  givenValPast <- NULL

  if (is.null(priorGrid)) {

    classPrior <- predClassPriorLc(
      model = model,
      dataSub = dataSub
    )

  } else {

    if (!is.list(priorGrid) || is.null(priorGrid$dist)) {
      stop("'priorGrid' must be a list with component 'dist'.")
    }

    givenValPast <- priorGrid$givenVal
    priorDist <- as.data.frame(priorGrid$dist)

    if (nrow(priorDist) != nClasses) {
      stop("'priorGrid$dist' must contain one row per latent class.")
    }

    classPrior <- as.numeric(priorDist[[ncol(priorDist)]])

    if (any(!is.finite(classPrior)) || any(classPrior < 0)) {
      stop("'priorGrid$dist' contains invalid class probabilities.")
    }

    if (sum(classPrior) <= 0) {
      stop("'priorGrid$dist' contains class probabilities that sum to zero.")
    }

    classPrior <- classPrior / sum(classPrior)

  }

  # (3) Posterior class distribution, conditional on known item responses

  classProbUse <- classPrior
  givenValAll <- givenValPast

  if (!is.null(givenVal)) {

    likelihood <- rep(1, nClasses)

    for (j in names(givenVal)) {

      ## response of item j (in column-major order; same as original)
      response <- givenVal[[j]]

      ## labels for item j
      varLabels_j <- varLabels[[j]]

      ## find closest category
      respCol <- which(
        abs(as.numeric(varLabels_j) - response) <=
          min(abs(as.numeric(varLabels_j) - response))
      )
      respCol <- respCol[1]

      ## item category probabilities conditional on latent variable
      probs <- as.matrix(itemProbs[[j]])
      if (!is.null(colnames(probs)) &&
          all(as.character(varLabels_j) %in% colnames(probs))) {
        probs <- probs[, as.character(varLabels_j), drop = FALSE]
      }

      ## update likelihood (conditional independence)
      likelihood <- likelihood * probs[, respCol]
    }

    classPost <- classPrior * likelihood

    if (sum(classPost) <= 0) {
      stop("Sum of the conditional class distribution is 0.")
    }

    classPost <- classPost / sum(classPost)
    classProbUse <- classPost

    givenValAll <- c(givenValPast, givenVal)

    postDistClass <- data.frame(
      class = seq_len(nClasses),
      postDistClass = classPost
    )

    outList$postDistLatent <- list(
      dist = postDistClass,
      givenVal = givenValAll
    )

  } else if (!is.null(givenValPast)) {

    givenValAll <- givenValPast
  }

  # (4) Exact full joint response distribution

  if (isTRUE(fullJoint)) {

    jointDist <- fullJointDistRespLc(
      classProb = classProbUse,
      itemProbs = itemProbs,
      varLabels = varLabels,
      items = items,
      givenValAll = givenValAll
    )

    outList$jointDist <- jointDist

    return(outList)
  }

  # (5) Simulate response value

  # latent classes
  set.seed(seed + 1)

  classSim <- sample(
    x = seq_len(nClasses),
    size = nSimLatent,
    replace = TRUE,
    prob = classProbUse
  )

  classSimRep <- rep(classSim, nSimItem)

  # Simulate response values given latent classes

  set.seed(seed + 1)

  respSim <- matrix(
    NA,
    nrow = length(classSimRep),
    ncol = length(items)
  )

  colnames(respSim) <- items

  for (j in items) {
    # Consider known values (if any)
    if (j %in% names(givenValAll)) {
      respSim[, j] <- givenValAll[[j]]
    } else{
      # Extract distribution of current item for every latent class
      probsItem <- as.matrix(itemProbs[[j]])

      # Extract item labels of current item
      labelsItem <- varLabels[[j]]

      # Ensure columns correspond to stored response labels
      if (!is.null(colnames(probsItem)) &&
          all(as.character(labelsItem) %in% colnames(probsItem))) {
        probsItem <- probsItem[, as.character(labelsItem), drop = FALSE]
      }

      # Simulate values for every latent class
      for (k in seq_len(nClasses)) {
        ## Extract rows where latent class k was sampled
        idx <- which(classSimRep == k)

        ## make simulations conditional on latent class k
        if (length(idx) > 0L) {
          respSim[idx, j] <- sample(
            x = labelsItem,
            size = length(idx),
            replace = TRUE,
            prob = probsItem[k, ]
          )
        }
      }
    }
  }

  respSim <- as.data.frame(respSim, stringsAsFactors = FALSE)

  respSim[, items] <- lapply(respSim[, items, drop = FALSE], facToNumeric)

  # (5) Approximate joint distribution of response combinations
  freqTable <- plyr::count(respSim)
  freqTable$freq <- freqTable$freq / sum(freqTable$freq)

  colnames(freqTable) <- c(items, "freq")

  # Make item values numeric
  freqTable[, items] <- lapply(
    freqTable[, items, drop = FALSE],
    facToNumeric
  )
  # Save results
  outList$sim <- respSim
  outList$jointDist <- freqTable

  return(outList)
}
