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


