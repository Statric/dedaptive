#' Check a user-supplied theta grid for prediction (helper function)
#'
#' @param thetaGrid Numeric matrix or object coercible to a matrix. The grid must
#'   contain one column per latent factor and one row per grid point.
#' @param nFactors Integer; number of latent variables.
#' @param factorNames Optional character vector with names of the latent
#'   variables. If supplied, these names are used as column names of the returned
#'   grid. If \code{NULL}, names are generated as \code{F1}, \code{F2}, etc.
#'
#' @return A numeric matrix containing the checked theta grid.
#' @export
#' @examples # no example, since it is only a helper function

checkThetaGridPred <- function(thetaGrid,
                               nFactors,
                               factorNames = NULL) {

  thetaGrid <- as.matrix(thetaGrid)

  if (!is.numeric(thetaGrid)) {
    stop("'thetaGrid' must be numeric.")
  }

  if (ncol(thetaGrid) != nFactors) {
    stop("'thetaGrid' must have one column per latent factor.")
  }

  if (any(!is.finite(thetaGrid))) {
    stop("'thetaGrid' must contain only finite values.")
  }

  if (is.null(factorNames)) {
    factorNames <- paste0("F", seq_len(nFactors))
  }

  if (length(factorNames) != nFactors) {
    stop("'factorNames' must have length equal to 'nFactors'.")
  }

  colnames(thetaGrid) <- factorNames

  return(thetaGrid)
}

#' Create a regular theta grid for prediction (helper function)
#'
#' @param nFactors Integer; number of latent variables.
#' @param thetaLim Numeric vector of length 2 defining the lower and upper limits
#'   of the grid for each latent dimension. Defaults to \code{c(-6, 6)}.
#' @param thetaQuadpts Optional integer specifying the number of grid points per
#'   latent dimension. If \code{NULL}, default values are chosen based on
#'   \code{nFactors}.
#' @param factorNames Optional character vector with names of the latent
#'   variables. If supplied, these names are used as column names of the returned
#'   grid. If \code{NULL}, names are generated as \code{F1}, \code{F2}, etc.
#'
#' @return A numeric matrix containing the theta grid. The matrix has one column
#'   per latent factor and one row per grid point.
#' @export
#' @examples # no example, since it is only a helper function

makeThetaGridPred <- function(nFactors,
                              thetaLim = c(-6, 6),
                              thetaQuadpts = NULL,
                              factorNames = NULL) {

  if (!is.numeric(nFactors) || length(nFactors) != 1L ||
      is.na(nFactors) || nFactors < 1L) {
    stop("'nFactors' must be a positive integer.")
  }

  nFactors <- as.integer(nFactors)

  if (is.null(factorNames)) {
    factorNames <- paste0("F", seq_len(nFactors))
  }

  if (length(factorNames) != nFactors) {
    stop("'factorNames' must have length equal to 'nFactors'.")
  }

  if (!is.numeric(thetaLim) || length(thetaLim) != 2L ||
      any(!is.finite(thetaLim)) || thetaLim[1] >= thetaLim[2]) {
    stop("'thetaLim' must be a numeric vector of length 2 with thetaLim[1] < thetaLim[2].")
  }

  if (is.null(thetaQuadpts)) {
    thetaQuadpts <- switch(
      as.character(nFactors),
      "1" = 61L,
      "2" = 31L,
      "3" = 15L,
      "4" = 9L,
      "5" = 7L,
      3L
    )
  }

  if (!is.numeric(thetaQuadpts) || length(thetaQuadpts) != 1L ||
      is.na(thetaQuadpts) || thetaQuadpts <= 2L ||
      thetaQuadpts != as.integer(thetaQuadpts)) {
    stop("'thetaQuadpts' must be a single integer larger than 2.")
  }

  thetaQuadpts <- as.integer(thetaQuadpts)

  thetaSeq <- seq(thetaLim[1], thetaLim[2], length.out = thetaQuadpts)

  thetaGrid <- as.matrix(
    expand.grid(rep(list(thetaSeq), nFactors))
  )

  colnames(thetaGrid) <- factorNames

  return(thetaGrid)
}

#' Get theta grid for prediction
#'
#' @param fit Fitted \code{\link[mirt]{mirt}} model object.
#' @param nFactors Integer; number of latent variables.
#' @param factorNames Optional character vector with names of the latent
#'   variables. If supplied, these names are used as column names of the returned
#'   grid. If \code{NULL}, names are generated as \code{F1}, \code{F2}, etc.
#' @param thetaLim Optional numeric vector of length 2 defining the lower and
#'   upper limits of the prediction grid. If \code{NULL}, default limits are used
#'   when a new regular grid is created.
#' @param thetaQuadpts Optional integer specifying the number of grid points per
#'   latent dimension. If \code{NULL}, default values are used when a new regular
#'   grid is created.
#' @param thetaGrid Optional numeric matrix containing a custom prediction grid.
#'   The matrix must have one column per latent factor. If supplied, this grid
#'   takes precedence over \code{thetaLim}, \code{thetaQuadpts}, and the internal
#'   theta grid from the fitted \code{\link[mirt]{mirt}} object.
#'
#' @return A numeric matrix containing the theta grid used for prediction.
#'
#' @export
#' @examples # no example, since it is only a helper function$

getThetaGridPred <- function(fit,
                             nFactors,
                             factorNames = NULL,
                             thetaLim = NULL,
                             thetaQuadpts = NULL,
                             thetaGrid = NULL) {

  # If user supplies a custom grid, validate and use it directly
  if (!is.null(thetaGrid)) {
    return(
      checkThetaGridPred(
        thetaGrid = thetaGrid,
        nFactors = nFactors,
        factorNames = factorNames
      )
    )
  }

  # If user supplies grid settings, create a prediction grid
  if (!is.null(thetaLim) || !is.null(thetaQuadpts)) {
    thetaLimUse <- if (is.null(thetaLim)) c(-6, 6) else thetaLim

    return(
      makeThetaGridPred(
        nFactors = nFactors,
        thetaLim = thetaLimUse,
        thetaQuadpts = thetaQuadpts,
        factorNames = factorNames
      )
    )
  }

  # If no grid settings are supplied, use the internal mirt grid if available
  thetaGridMirt <- fit@Model$Theta

  if (!is.null(thetaGridMirt) &&
      nrow(thetaGridMirt) > 0L &&
      ncol(thetaGridMirt) == nFactors) {

    return(
      checkThetaGridPred(
        thetaGrid = thetaGridMirt,
        nFactors = nFactors,
        factorNames = factorNames
      )
    )
  }

  # Fallback: mimic the usual EM-style grid
  makeThetaGridPred(
    nFactors = nFactors,
    thetaLim = c(-6, 6),
    factorNames = factorNames
  )
}

#' Check fitted IRT model object for prediction functions (helper function)
#'
#' @param model Object to check. Usually a list returned by \code{\link{fitIrt}}.
#'
#' @return Invisibly returns \code{TRUE} if all required components are present.
#' Otherwise, an informative error is returned.
#' @export
#' @examples # no example, since it is only a helper function

checkIrtPredModel <- function(model) {

  if (!is.list(model)) {
    stop("'model' must be an object returned by fitIrt().")
  }

  requiredTop <- c(
    "items",
    "varLabels",
    "coef",
    "nFactors",
    "thetaGridPred",
    "itemProbs"
  )

  missingTop <- requiredTop[
    vapply(requiredTop, function(x) is.null(model[[x]]), logical(1))
  ]

  if (length(missingTop) > 0L) {
    stop(
      "'model' is missing required component(s): ",
      paste(missingTop, collapse = ", "),
      ". Please refit the model with fitIrt()."
    )
  }

  if (!is.list(model$coef)) {
    stop("'model$coef' must be a list.")
  }

  requiredCoef <- c(
    "paramItems",
    "slopesItems",
    "intItems",
    "thetaCovPrior"
  )

  missingCoef <- requiredCoef[
    vapply(requiredCoef, function(x) is.null(model$coef[[x]]), logical(1))
  ]

  if (length(missingCoef) > 0L) {
    stop(
      "'model$coef' is missing required component(s): ",
      paste(missingCoef, collapse = ", "),
      ". Please refit the model with fitIrt()."
    )
  }

  invisible(TRUE)
}

#' Cost-based binary classifier (helper function)
#'
#' @param prob Probability of higher class.
#' @param missCosts Vector of misclassification costs (costs for false positive,
#' costs for false negative).
#' @param class1 Name of lower class.
#' @param class2 name of higher class.
#'
#' @return classifier
#' @export
#'
#' @examples # no example, since it is only a helper function
#'
fcClassFct <- function(prob,
                       missCosts,
                       class1,
                       class2) {
  # Check format of missclassification costs
  if (length(missCosts) != 2) {
    stop("'missCosts' must be a numeric vector of length 2.")
  }

  if (any(missCosts < 0)) {
    stop("'missCosts' must contain nonnegative costs.")
  }

  if (sum(missCosts) <= 0) {
    stop("The sum of 'missCosts' must be positive.")
  }

  # Compute classifier
  zHat <- rep(class2, length(prob))
  zHat[prob < missCosts[1] / (missCosts[1] + missCosts[2])] <- class1

  return(zHat)
}

#' Compute conditional joint distribution (helper function)
#'
#' @param jointDist table of the joint distribution.
#' @param varName Name of variable we want to condition on.
#' @param varValue Given value of \code{varName}.
#' @param nameFreq Name of column that contains the frequency.
#'
#' @return conditional joint distribution
#' @export
#'
#' @examples # no example, since it is only a helper function
multiMultinomCondFromJoint <- function(jointDist,
                                       varName,
                                       varValue,
                                       nameFreq = "freq") {

  # Checks
  if (!(varName %in% names(jointDist))) {
    stop("'varName' must be a column in 'jointDist'.")
  }

  if (!(nameFreq %in% names(jointDist))) {
    stop("'nameFreq' must be a column in 'jointDist'.")
  }

  # Filter probabilities with Y_m = l
  jointMl <- jointDist[jointDist[[varName]] == varValue, ]

  # P(Y_m = l)
  probMl <- sum(jointMl[[nameFreq]])
  if (probMl <= 0) {
    stop("Conditioning event has probability 0.")
  }

  # Conditional joint distribution of remaining variables
  jointMlCond <- jointMl
  jointMlCond[[nameFreq]] <- jointMl[[nameFreq]] / probMl
  jointMlCond <- jointMlCond[, colnames(jointMlCond) != varName, drop = FALSE]

  return(list(probValue = probMl, cond = jointMlCond))
}


#' Predictions based on joint distribution of items and corresponding scores (helper function)
#'
#' @param jointDistFun Joint distribution of item responses patterns
#' that also contains the scores as columns.
#' @param thres thresholds for decisions based on the scores.
#'
#' @return table of predictions based on the joint distribution
#' @export
#'
#' @examples # no example, since it is only a helper function
predFromJoint <- function(jointDistFun,
                          thres) {
  # Checks
  if (length(thres) == 0) {
    stop("'thres' must contain at least one threshold.")
  }
  if (!"freq" %in% names(jointDistFun)) {
    stop("'jointDistFun' must contain a 'freq' column.")
  }

  # Create table with frequencies of function values
  ## formula for aggregation
  funNames  <- paste("fun",  seq_along(thres), sep = "_")
  diagNames <- paste("diag", seq_along(thres), sep = "_")

  if (!all(funNames %in% names(jointDistFun))) {
    stop("All required 'fun_*' columns must be present in 'jointDistFun'.")
  }

  if (!all(diagNames %in% names(jointDistFun))) {
    stop("All required 'diag_*' columns must be present in 'jointDistFun'.")
  }

  formAg <- stats::as.formula(
    paste0("freq ~ ", paste(c(funNames, diagNames), collapse = "+"))
  )

  ## aggregate frequencies
  distFun <- stats::aggregate(formAg, jointDistFun, FUN = sum)

  # Compute expected value and probability that function is higher than the threshold
  predMean  <- numeric(length(thres))
  probThres <- numeric(length(thres))

  for (i in seq_along(thres)) {
    f <- funNames[i]
    predMean[i]  <- sum(distFun[[f]] * distFun$freq)
    probThres[i] <- sum(jointDistFun$freq[jointDistFun[[f]] >= thres[i]])
  }

  names(predMean)  <- paste("predMean", seq_along(thres), sep = "_")
  names(probThres) <- paste("prob", seq_along(thres), sep = "_")

  dPred <- data.frame(as.list(c(predMean, probThres)))

  return(list(distFun = distFun, pred = dPred))
}

#' Cost-based classification for multiple binary decisions (helper function)
#'
#' @param probDiag Joint distribution of the decisions.
#' @param cFp Vector of costs of false positives (for every decision).
#' @param cFn Vector of costs of false negatives (for every decision).
#' @param probName Name of the column containing the probabilities.
#'
#' @return classifications and expected costs of classifications
#' @export
#'
#' @examples # no example, since it is only a helper function
classMultBinDiag <- function(probDiag,
                             cFp,
                             cFn,
                             probName = "freq") {
  # Checks
  if (length(cFp) != length(cFn)) {
    stop("'cFp' and 'cFn' must have the same length.")
  }

  if (!(probName %in% names(probDiag))) {
    stop("'probName' must be a column in 'probDiag'.")
  }

  # Define diagnosis and classifiers names
  diagNames <- colnames(probDiag)[colnames(probDiag) != probName]
  predDiagNames <- paste("predDiag", 1:length(cFp), sep = "_")

  if (length(cFp) != length(diagNames)) {
    stop("Lengths of 'cFp' and 'cFn' must match the number of diagnosis columns.")
  }

  # all combinations of diagnoses and predicted diagnoses
  gridDiag <- expand.grid(rep(list(c(0, 1)), length(cFp) * 2))
  colnames(gridDiag) <- c(diagNames, predDiagNames)

  gridDiag <- plyr::join(gridDiag, probDiag, by = diagNames)

  posComb <- apply(probDiag[, diagNames, drop = FALSE], 1,
                   function(x) paste(x, collapse = "_"))

  gridDiagIn            <- gridDiag
  gridDiagIn$diagAll    <- apply(gridDiagIn[, diagNames, drop = FALSE], 1,
                                 function(x) paste(x, collapse = "_"))
  gridDiagIn$predAll    <- apply(gridDiagIn[, predDiagNames, drop = FALSE], 1,
                                 function(x) paste(x, collapse = "_"))

  gridDiag <- gridDiag[
    gridDiagIn$diagAll %in% posComb & gridDiagIn$predAll %in% posComb,
  ]

  gridDiag <- stats::na.omit(gridDiag)

  # Compute costs for every diagnosis separately for all combinations
  for (f in seq_along(cFp)) {
    gridDiag[[paste0("costs_", f)]] <-
      cFp[f] * (gridDiag[[diagNames[f]]] == 0) *
      (gridDiag[[paste0("predDiag_", f)]] == 1) +
      cFn[f] * (gridDiag[[diagNames[f]]] == 1) *
      (gridDiag[[paste0("predDiag_", f)]] == 0)
  }

  # Compute costs over all diagnoses and the expected costs for every combination
  gridDiag[["costs"]]   <- rowSums(gridDiag[, grepl("cost", colnames(gridDiag)),
                                            drop = FALSE])
  gridDiag$expCosts     <- gridDiag$costs * gridDiag[[probName]]

  formAg <- stats::as.formula(
    paste0("expCosts ~ ", paste(predDiagNames, collapse = "+"))
  )

  tabCostClass <- stats::aggregate(formAg, data = gridDiag, FUN = sum)

  expCostClass <- min(tabCostClass$expCosts)

  predDiag <- tabCostClass[
    which(tabCostClass$expCosts <= expCostClass),
    grepl("^predDiag", colnames(tabCostClass)),
    drop = FALSE
  ]

  predDiag <- predDiag[1, , drop = FALSE]
  predDiag$expCost <- expCostClass

  return(predDiag)
}


