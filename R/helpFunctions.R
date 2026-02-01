#' Cost-based binary classifier (helping function)
#'
#' @param prob Probability of higher class
#' @param missCosts Vector of misclassification costs (costs for false positive, costs for false negative)
#' @param class1 Name of lower class
#' @param class2 name of higher class
#'
#' @returns classifier
#' @export
#'
#' @examples # no example, since it is only a helping function
#'
fcClassFct <- function(prob, missCosts, class1, class2) {
  zHat <- ifelse(
    prob < missCosts[1] / (missCosts[1] + missCosts[2]),
    class1,
    class2
  )
  return(zHat)
}

#' Compute conditional joint distribution (helping function)
#'
#' @param jointDist table of the joint distribution
#' @param varName Name of variable we want to condition
#' @param varValue Given value of 'varName'
#' @param nameFreq Name of column that contains the frequency
#'
#' @returns conditional joint distribution
#' @export
#'
#' @examples # no example, since it is only a helping function
multiMultinomCondFromJoint <- function(jointDist, varName, varValue, nameFreq = "freq") {

  # Filter probabilities with Y_m = l
  jointMl <- jointDist[jointDist[[varName]] == varValue, ]

  # P(Y_m = l)
  probMl <- sum(jointMl[[nameFreq]])

  # Conditional joint distribution of remaining variables
  jointMlCond <- jointMl
  jointMlCond[[nameFreq]] <- jointMl[[nameFreq]] / probMl
  jointMlCond <- jointMlCond[, -which(colnames(jointMlCond) %in% varName)]

  return(list(probValue = probMl, cond = jointMlCond))
}


#' Make predictions based on joint distribution of items and corresponding scores (helping function)
#'
#' @param jointDistFun Joint distribution that also contains the scores as columns
#' @param thres thresholds for decisions based on the scores
#'
#' @returns table of predictions based on the joint distribution
#' @export
#'
#' @examples # no example, since it is only a helping function
predFromJoint <- function(jointDistFun, thres) {

  # Create table with frequencies of function values
  ## formula for aggregation
  funNames  <- paste("fun",  1:length(thres), sep = "_")
  diagNames <- paste("diag", 1:length(thres), sep = "_")

  formAg <- stats::as.formula(
    paste0("freq ~ ", paste(c(funNames, diagNames), collapse = "+"))
  )

  ## aggregate frequencies
  distFun <- stats::aggregate(formAg, jointDistFun, FUN = sum)

  # Compute expected value and probability that function is higher than the threshold
  predMean  <- NULL
  probThres <- NULL

  for (i in seq_along(thres)) {
    f <- funNames[i]
    predMean  <- c(predMean,  sum(distFun[[f]] * distFun$freq))
    probThres <- c(probThres,
                   sum(jointDistFun$freq[jointDistFun[[f]] >= thres[i]]))
  }

  names(predMean)  <- paste("predMean", 1:length(thres), sep = "_")
  names(probThres) <- paste("prob",     1:length(thres), sep = "_")

  dPred <- data.frame(as.list(c(predMean, probThres)))

  return(list(distFun = distFun, pred = dPred))
}

#' Title cost-based classification for multiple binary decisions (helping function)
#'
#' @param probDiag table of probabilities for the decisions
#' @param cFp vector of costs of false positives (for every decision)
#' @param cFn vector of costs of false negatives (for every decisions)
#' @param probName Name of columns containing the probabilities
#'
#' @returns classifications and expected costs of classifications
#' @export
#'
#' @examples # no example, since it is only a helping function
classMultBinDiag <- function(probDiag, cFp, cFn, probName = "freq") {
  # Define diagnosis and classifiers names
  diagNames     <- colnames(probDiag)[colnames(probDiag) != probName]
  predDiagNames <- paste("predDiag", 1:length(cFp), sep = "_")

  # all combinations of diagnoses and predicted diagnoses
  gridDiag <- expand.grid(
    data.frame(matrix(rep(c(0, 1), length(cFp) * 2), ncol = length(cFp) * 2))
  )
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

  # Compute costs for every diagnosis seperatly for all combinations
  for (f in seq_along(cFp)) {
    gridDiag[[paste0("costs_", f)]] <-
      cFp[f] * (gridDiag[[diagNames[f]]] == 0) *
      (gridDiag[[paste0("predDiag_", f)]] == 1) +
      cFn[f] * (gridDiag[[diagNames[f]]] == 1) *
      (gridDiag[[paste0("predDiag_", f)]] == 0)
  }

  # Compute costs over all diagnoses and the expected costs for every combination
  gridDiag[["costs"]]   <- rowSums(gridDiag[, grepl("cost", colnames(gridDiag))])
  gridDiag$expCosts     <- gridDiag$costs * gridDiag[[probName]]

  formAg <- stats::as.formula(
    paste0("expCosts ~ ", paste(predDiagNames, collapse = "+"))
  )

  tabCostClass <- stats::aggregate(formAg, data = gridDiag, FUN = sum)

  expCostClass <- min(tabCostClass$expCosts)
  expCostClass <- expCostClass[1]

  predDiag <- tabCostClass[
    which(tabCostClass$expCosts <= expCostClass),
    grepl("predDiag", colnames(tabCostClass))
  ]

  predDiag <- predDiag[1, , drop = FALSE]
  predDiag$expCost <- expCostClass

  return(predDiag)
}


