#' DEcision-oriented aDAPTIVE (dedaptive) testing based on a Multidimensional Item Response Theory model
#'
#' @description
#' \code{dedaptiveIrt()} performs DEcision-oriented aDAPTIVE (dedaptive) testing
#' based on a Multidimensional Item Response Theory (MIRT) model fitted with
#' \code{\link{fitIrt}} and probabilistic predictions from \code{\link{predJointDistRespIrt}}.
#' For a given person, items are selected sequentially to minimize expected total
#' costs of misclassification and measurement.
#'
#' @details
#' Dedaptive testing combines:
#' \itemize{
#'   \item score functions of item responses (\code{funOfItems}),
#'   \item thresholds defining decisions on these scores (\code{thres}), and
#'   \item misclassification and measurement costs (\code{costs}).
#' }
#' The lengths of these arguments must be consistent: \code{funOfItems},
#' \code{thres}, \code{costs[[1]]} (false positive costs), and
#' \code{costs[[2]]} (false negative costs) must all have the same length
#' (one score/decision per entry). \code{costs[[3]]} is a single scalar
#' specifying the per-item measurement cost.
#'
#' @param model MIRT model object fitted with \code{\link{fitIrt}}.
#' @param predJointSub Optional object containing the predicted distribution of
#'   response patterns for the current person, as returned
#'   by \code{\link{predJointDistRespIrt}}. If \code{NULL}, this distribution
#'   is computed inside the function before any item is selected.
#' @param dataSub One-row data frame for the current person containing item
#'   responses and, if applicable, predictor variables used in the latent
#'   regression of the MIRT model.
#' @param funOfItems List of functions used to compute score(s) from the item
#'   responses (e.g., \code{list(sum)} for a sum score over all items, or several functions
#'   for multiple decisions).
#' @param thres Numeric vector of thresholds applied to the scores computed by
#'   \code{funOfItems} to define binary decisions (one threshold per score
#'   function). The length of \code{thres} must match the length of \code{funOfItems}.
#' @param costs List of length 3 with cost parameters:
#'   \itemize{
#'     \item \code{costs[[1]]}: numeric vector of false positive costs, one per
#'       decision,
#'     \item \code{costs[[2]]}: numeric vector of false negative costs, one per
#'       decision,
#'     \item \code{costs[[3]]}: scalar measurement cost per selected item.
#'   }
#'   The lengths of \code{costs[[1]]} and \code{costs[[2]]} must match the
#'   length of \code{funOfItems} respectively \code{thres}.
#' @param itemsExclude Character vector of item names that cannot be selected.
#'   If \code{NULL} (default), all items are eligible for selection.
#' @param nSimTheta Number of latent variable draws used internally in
#'   \code{\link{predJointDistRespIrt}}.
#' @param nSimItem Number of response patterns simulated per latent draw used internally
#' in \code{\link{predJointDistRespIrt}}.
#' @param seed Integer seed used to make the sequential selection procedure and
#'   simulations reproducible.
#' @param saveSteps Logical; if \code{TRUE}, the output from each step of
#'   the sequential selection procedure is saved. If \code{FALSE} (default),
#'   only the final results are returned.
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
#'   \item{\code{outSteps}}{If \code{saveSteps = TRUE}, a list containing
#'     intermediate results from each step of the sequential selection
#'     procedure is returned.}
#' }
#'
#' @import mirt
#' @export

dedaptiveIrt <- function(model = NULL,
                         predJointSub = NULL,
                         dataSub,
                         funOfItems = list(sum),
                         thres,
                         costs,
                         itemsExclude = NULL,
                         nSimTheta = 1000,
                         nSimItem = 10,
                         seed = 131820,
                         saveSteps = FALSE) {

  # (1) Preparation
  # Checks

  checkIrtPredModel(model)

  if (!is.data.frame(dataSub) || nrow(dataSub) != 1L) {
    stop("'dataSub' must be a one-row data frame.")
  }

  if (!is.list(funOfItems) || length(funOfItems) == 0L) {
    stop("'funOfItems' must be a non-empty list of functions.")
  }

  if (length(thres) != length(funOfItems)) {
    stop("'thres' must have the same length as 'funOfItems'.")
  }

  if (!is.list(costs) || length(costs) != 3L) {
    stop("'costs' must be a list of length 3.")
  }

  if (length(costs[[1]]) != length(funOfItems) ||
      length(costs[[2]]) != length(funOfItems)) {
    stop("Lengths of 'costs[[1]]' (false positives) and 'costs[[2]]' (false negatives) must match 'funOfItems'.")
  }

  # Time stamp at the beginning of the procedure
  timeStamp1 <- Sys.time()

  # Extract information from the IRT model
  items <- model$items

  if(!is.null(itemsExclude)) {
    itemsAllowed<- setdiff(items, itemsExclude)
  } else {
    itemsAllowed<- items
  }
  nResp <- length(itemsAllowed)
  startJoint <- !is.null(predJointSub)

  # Generate individual seeds
  set.seed(seed)
  seeds <- sample.int(.Machine$integer.max, nResp)

  # Extract cost parameters
  cFp <- costs[[1]]  # false positive costs for each decision
  cFn <- costs[[2]]  # false negative costs for each decision
  cM  <- costs[[3]]  # measurement cost per selected item

  # Compute the initial joint distribution if not provided
  if (is.null(predJointSub)) {
    predJointSub <- predJointDistRespIrt(
      model = model,
      dataSub = dataSub,
      nSimTheta = nSimTheta,
      nSimItem = nSimItem,
      seed = seed
    )
  }

  # Extract only the joint distribution of item responses
  predJointSub <- predJointSub$jointDist

  # Compute score functions and binary decisions for each simulated response pattern
  for (f in seq_along(funOfItems)) {
    # Scores (functions of item responses)
    predJointSub[[paste0("fun_", f)]] <-
      apply(predJointSub[, items, drop = FALSE],
            1, funOfItems[[f]])

    # Binary decisions based on thresholds
    predJointSub[[paste0("diag_", f)]] <-
      ifelse(predJointSub[[paste0("fun_", f)]] >= thres[f], 1, 0)
  }

  # Initialize quantities for the sequential selection procedure
  ## current joint distribution given selected items
  predJointSubTemp <- predJointSub
  ## vector of selected item names
  itemsChosen <- NULL
  ## indicator whether adding an item reduces costs
  costRed <- TRUE
  ## set of remaining (not yet selected) items
  itemsLeft <- itemsAllowed
  ## latent distribution from the last step
  distThetaPast <- NULL

  ## output from every step
  if(saveSteps){outSteps<- list()}

  stepCount <- 0
  while (costRed && length(itemsLeft) > 0) {
    stepCount <- stepCount + 1

    # (2) Expected costs given past measurements

    # Compute joint distribution of diagnoses
    probDiagPast <- stats::aggregate(
      stats::as.formula(
        paste0("freq ~ ",
               paste(paste("diag", seq_along(funOfItems), sep = "_"),
                     collapse = "+"))
      ),
      data = predJointSubTemp,
      FUN = sum
    )

    # Expected misclassification costs given the current state (no new item)
    expCostsPast <- classMultBinDiag(probDiagPast, costs[[1]], costs[[2]])$expCost

    # (3) Expected costs when adding each of the remaining items

    # Vector of expected total costs for each candidate item
    expCostPros <- NULL
    expCostProsPerLevel <- NULL

    if (length(itemsLeft) > 1) {
      expCostPros <- numeric(length(itemsLeft))

      if(saveSteps){
        expCostProsPerLevel <- list()
      }

      for (iItem in seq_along(itemsLeft)) {
        m <- itemsLeft[iItem]

        # Extract possible response levels for item m
        levelM <- sort(unique(predJointSubTemp[[m]]))

        # expected costs conditional on each level of item m
        expCostM <- numeric(length(levelM))

        # probabilities P(Y_m = l)
        probM <- numeric(length(levelM))

        for (iLevel in seq_along(levelM)) {
          l <- levelM[iLevel]

          # Probability P(Y_m = l) under the current joint distribution
          probMl <- sum(predJointSubTemp$freq[predJointSubTemp[[m]] == l])

          if (probMl > 0) {

            # Conditional joint distribution given Y_m = l
            jointMlCond <- multiMultinomCondFromJoint(predJointSubTemp, m, l)
            probMl <- jointMlCond$probValue
            jointMlCond <- jointMlCond$cond

            # Joint distribution of diagnoses conditional on Y_m = l
            probDiagMl <- stats::aggregate(
              stats::as.formula(
                paste0("freq ~ ",
                       paste(paste("diag", seq_along(funOfItems), sep = "_"),
                             collapse = "+"))
              ),
              data = jointMlCond,
              FUN = sum
            )

            # Expected misclassification costs in this conditional distribution
            expCostsMl <- classMultBinDiag(probDiagMl, costs[[1]], costs[[2]])$expCost

          } else {
            # If P(Y_m = l) = 0, the corresponding cost contribution is 0
            expCostsMl <- 0
          }
          # Add P(Ym=l) and expected costs to vector
          probM[iLevel] <- probMl
          expCostM[iLevel] <- expCostsMl
        }

        # Table with probabilities and expected costs for item m
        costTabM <- cbind(probM, expCostM)
        rownames(costTabM) <- levelM
        if(saveSteps){
          expCostProsPerLevel[[m]]<- costTabM
        }

        # Expected total cost when including item m (including measurement cost)
        exCostsM <- sum(costTabM[, 1] * costTabM[, 2]) + cM
        expCostPros[iItem] <- exCostsM
      }

      names(expCostPros) <- itemsLeft

      # (4) Termination and item selection rules

      # Identify the item with minimal expected costs
      itemMinCosts <- which(expCostPros <= min(expCostPros))
      itemMinCosts <- itemMinCosts[1]
      minExpCost <- as.numeric(expCostPros[itemMinCosts])
      itemNameMinCost <- names(expCostPros)[itemMinCosts]

      # Difference between expected cost with and without selecting another item
      diffExpCost <- min(c(minExpCost, length(itemsLeft) * cM)) - expCostsPast

    } else {
      # If only one item is left, we compare its measurement cost to the current costs
      itemNameMinCost <- itemsLeft
      diffExpCost <- cM - expCostsPast
    }

    # Termination condition: stop if no cost reduction is achieved
    costRed <- diffExpCost < 0
    if(saveSteps){
      outSteps[[stepCount]] <- list(dist=predJointSubTemp,
                                    expCostsPast=expCostsPast,
                                    expCostPros=expCostPros,
                                    expCostProsPerLevel=expCostProsPerLevel,
                                    itemNameMinCost=itemNameMinCost,
                                    diffExpCost=diffExpCost)
    }
    if (costRed) {

      # (5) Update distributions after selecting the best item

      # Add the selected item to the list of chosen items
      itemsChosen <- c(itemsChosen, itemNameMinCost)

      # Remove item from the list of left-over items
      itemsLeft <- setdiff(itemsAllowed, itemsChosen)

      # Observed value of the last selected item
      valItemLast <- as.numeric(dataSub[, itemNameMinCost])
      names(valItemLast) <- itemNameMinCost

      # Update the joint distribution of remaining items conditional on the last item
      predJointSubTemp <- predJointDistRespIrt(
        model = model,
        dataSub = dataSub,
        nSimTheta = nSimTheta,
        nSimItem = nSimItem,
        seed = seeds[stepCount],
        givenVal = valItemLast,
        priorGrid = distThetaPast
      )

      # Use the posterior distribution of the latent variables as prior for next step
      distThetaPast <- predJointSubTemp$postDistTheta
      predJointSubTemp <- predJointSubTemp$jointDist

      # Recompute scores and diagnoses for the remaining items
      for (f in seq_along(funOfItems)) {
        predJointSubTemp[[paste0("fun_", f)]] <-
          apply(predJointSubTemp[, items,
                                 drop = FALSE],
                1, funOfItems[[f]])

        predJointSubTemp[[paste0("diag_", f)]] <-
          ifelse(predJointSubTemp[[paste0("fun_", f)]] >= thres[f], 1, 0)
      }

      # Remove columns of already selected items from the joint distribution
      predJointSubTemp <- predJointSubTemp[, !(colnames(predJointSubTemp) %in%
                                                 itemsChosen),
                                           drop = FALSE
      ]
    }
  }

  # (6) Final predictions based on the selected item set

  if (length(itemsChosen) != length(items)) {
    # Predict distribution of score functions and summary measures
    out <- predFromJoint(predJointSubTemp, thres)
  } else {
    # In case all items are selected, we prepare to fill in the true values
    distFun <- matrix(NA, nrow = 1, ncol = length(funOfItems) + 1)
    distFun <- data.frame(distFun)
    colnames(distFun) <- c(paste("fun", seq_along(funOfItems), sep = "_"), "freq")
    distFun$freq <- 1

    dataPred <- matrix(NA, nrow = 1, ncol = length(funOfItems) * 2)
    dataPred <- data.frame(dataPred)
    colnames(dataPred) <- c(
      paste("predMean", seq_along(funOfItems), sep = "_"),
      paste("prob", seq_along(funOfItems), sep = "_")
    )

    out <- list(distFun = distFun, pred = dataPred)
  }

  # Compute true score values and decisions based on all item responses
  for (f in seq_along(funOfItems)) {
    out$pred[[paste0("trueMean_", f)]] <-
      funOfItems[[f]](dataSub[, items])

    out$pred[[paste0("diag_", f)]] <-
      ifelse(out$pred[[paste0("trueMean_", f)]] >= thres[f], 1, 0)

    # if all items are assessed compute true values
    if (length(itemsChosen) == length(items)) {
      out$pred[[paste0("predMean_", f)]] <-
        out$pred[[paste0("trueMean_", f)]]
      out$pred[[paste0("prob_", f)]] <-
        out$pred[[paste0("diag_", f)]]
      out$distFun[[paste0("fun_", f)]] <-
        out$pred[[paste0("trueMean_", f)]]
      out$distFun[[paste0("diag_", f)]] <-
        ifelse(out$distFun[[paste0("fun_", f)]] >= thres[f], 1, 0)
    }
  }

  # Add number of selected items and the item combination to the output
  out$pred$nItems <- length(itemsChosen)
  out$pred$combItems <- paste(itemsChosen, collapse = ", ")
  out$chosen <- itemsChosen

  # Add run time information
  timeStamp2 <- Sys.time()
  out$pred$runTime <- difftime(timeStamp2, timeStamp1, units = "secs")[[1]]
  denomRunTime<- out$pred$nItems + 1 - startJoint
  if(length(itemsChosen) >= length(itemsAllowed)) {denomRunTime<- denomRunTime - 1}
  out$pred$runTimePerItem <- out$pred$runTime / denomRunTime
  if(length(itemsChosen) <= 0) {
    out$pred$runTimePerItem <- out$pred$runTime
  }
  # Add joint distribution of not chosen items and distribution of latent variables
  out$distItems <- predJointSubTemp
  out$distTheta <- distThetaPast

  # Add the score functions, thresholds and costs as meta-data
  out$funOfItems <- funOfItems
  out$thres <- thres
  out$costs <- costs

  # Add output from every step
  if(saveSteps){out$outSteps<- outSteps}

  return(out)
}
