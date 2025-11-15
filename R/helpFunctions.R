# internal: cost-based binary classifier
.fcClassFct <- function(prob, missCosts, class1, class2) {
  zHat <- ifelse(
    prob < missCosts[1] / (missCosts[1] + missCosts[2]),
    class1,
    class2
  )
  return(zHat)
}

# internal: condition joint distribution on Y_m = l
.multiMultinomCondFromJoint <- function(jointDist, varName, varValue, nameFreq = "freq") {

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

# internal: compute predictions from joint distribution of fun/diag
.predFromJoint <- function(jointDistFun, thres) {

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

# internal: cost-based classification for multiple binary diagnoses
.classMultBinDiag <- function(probDiag, cFp, cFn, probName = "freq") {
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


