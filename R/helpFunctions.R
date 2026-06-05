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
#' @noRd
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

#' Get theta grid for prediction (helper function)
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
#' @noRd
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

#' Convert factor to numeric variable (helper function)
#'
#' @param x Factor vector
#'
#' @return Numeric vector
#'
#' @noRd
facToNumeric <- function(x) as.numeric(as.character(x))

#' Check fitted IRT or LC model object for prediction functions (helper function)
#'
#' @param model Object to check. Usually a list returned by \code{\link{fitIrt}}
#' or \code{\link{fitLc}}.
#'
#' @return Invisibly returns \code{TRUE} if all required components are present.
#' Otherwise, an informative error is returned.
#'
#' @noRd
checkPredModel <- function(model) {

  # Check if model is a list and contains modelType
  if (!is.list(model) || is.null(model$modelType) ||
      !(model$modelType %in% c("irt", "lc"))) {
    stop("'model' must be an object returned by fitIrt() or fitLc().")
  }

  # Check if model$coef is a list
  if (!is.list(model$coef)) {
    stop("'model$coef' must be a list.")
  }

  # Check coefficients and additional entries in model
  if (model$modelType == "irt") {
    ## Coefficients For IRT model
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

    ## Additional entries for IRT model
    requiredTop <- c(
      "items",
      "varLabels",
      "coef",
      "nFactors",
      "thetaGridPred",
      "itemProbs"
    )

  } else {

    ## Coefficients For LC model
    if (is.null(model$formula)) {
      if (is.null(model$coef$classPrior)) {
        stop("'model$coef$classPrior' is missing.")
      }
    } else {
      if (is.null(model$coef$paramReg)) {
        stop("'model$coef$paramReg' is missing for latent class regression.")
      }
      if (is.null(model$covFormula)) {
        stop("'model$covFormula' is missing for latent class regression.")
      }
    }

    # if(is.null(model$formula)) {
    #   if(is.null(model$coef$classPrior)) {
    #     stop("'model$coef$classPrior' cannot be NULL if 'model$formula' is NULL")
    #   }
    # } else {
    #   if(is.null(model$coef$paramReg)) {
    #     stop("'model$coef$paramReg' cannot be NULL if 'model$formula' is not NULL")
    #   }
    # }

    ## Additional entries for LC model
    requiredTop <- c(
      "items",
      "varLabels",
      "nClasses",
      "fit",
      "itemProbs",
      "coef"
    )
  }

  missingTop <- requiredTop[
    vapply(requiredTop, function(x) is.null(model[[x]]), logical(1))
  ]

  if (length(missingTop) > 0L) {
    stop(
      "'model' is missing required component(s): ",
      paste(missingTop, collapse = ", "),
      "."
    )
  }

  if (!is.character(model$items) || length(model$items) == 0L) {
    stop("'model$items' must be a non-empty character vector.")
  }

  if (!is.list(model$itemProbs)) {
    stop("'model$itemProbs' must be a list.")
  }

  if (!all(model$items %in% names(model$itemProbs))) {
    stop("'model$itemProbs' must contain one element for every item in 'model$items'.")
  }

  invisible(TRUE)
}

#' Search best latent-variable models over varying latent dimensionality/classes
#' (helper function)
#'
#' @description
#' Helper used by \code{\link{fitIrtSearchLatent}} and
#' \code{\link{fitLcSearchLatent}}. It fits several models with different numbers of
#' latent variables or latent classes and selects the best model according to
#' Akaike information criterion (AIC) or Bayesian Information criterion (BIC).
#' It uses the functions \code{\link{fitIrt}} or \code{\link{fitLc}}.
#'
#' @param items Character vector with the names of the columns in \code{data} containing
#' the item responses. These columns are used to fit the multidimensional graded
#' item response theory or unrestricted latent class model.
#' @param formula Either \code{NULL} (no latent regression), a character string
#'  containing only the right-hand side of a regression formula
#'  (e.g., \code{"age + sex"}), or a one-sided formula (e.g., \code{~ age + sex})
#'  specifying the predictors for the latent regression.
#' @param data A data frame containing the item responses specified in
#'  \code{items} and, if \code{formula} is not \code{NULL}, all predictor
#'  variables referenced in \code{formula}. Each row typically corresponds to
#'  one person.
#' @param nLatent Integer vector with the numbers of latent variables to search
#'   over.
#' @param aic Logical; if \code{TRUE}, the model with the lowest AIC is selected.
#'   If \code{FALSE}, the model with the lowest BIC is selected.
#' @param modelType Specification which model type should be used. Currently,
#' either \code{"irt"} (based on \code{\link[mirt]{mirt}}) or \code{"lc"}
#' (based on \code{\link[poLCA]{poLCA}}) are available.
#'
#' @param ... Further arguments passed to \code{\link{fitIrt}} or \code{\link{fitLc}}.
#'
#' @return Selected model object as in \code{\link{fitIrt}} or \code{\link{fitLc}}
#' with an additional \code{search} component.
#'
#' @export
#' @examples # no example, since it is only a helper function
#'
searchLatent <- function(items,
                         formula = NULL,
                         data,
                         nLatent = 1:5,
                         aic = FALSE,
                         modelType = c("irt", "lc"),
                         ...) {

  # (1) Preparation
  # Checks

  ## Data
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }

  ## Items
  if (!is.character(items) || length(items) == 0L) {
    stop("'items' must be a non-empty character vector.")
  }

  if (!all(items %in% names(data))) {
    stop("All variables listed in 'items' must be columns in 'data'.")
  }

  ## Model type
  modelType <- match.arg(modelType)

  ## Range of the number of latent variables/classes
  if (!is.numeric(nLatent) || length(nLatent) < 1L ||
      any(is.na(nLatent)) || any(nLatent < 1) ||
      any(nLatent != as.integer(nLatent))) {
    stop("'nLatent' must be a positive integer vector.")
  }

  nLatent <- sort(unique(as.integer(nLatent)))

  ## Criterion
  if (!is.logical(aic) || length(aic) != 1L || is.na(aic)) {
    stop("'aic' must be TRUE or FALSE.")
  }

  criterion <- if (isTRUE(aic)) "aic" else "bic"

  ## Additional arguments
  dots <- list(...)

  if (modelType == "irt" && "model" %in% names(dots)) {
    stop(
      "Do not pass 'model' through '...'. ",
      "The IRT model dimension is controlled by 'nLatent'."
    )
  }

  if (modelType == "lc" && "nClasses" %in% names(dots)) {
    stop(
      "Do not pass 'nClasses' through '...'. ",
      "The number of latent classes is controlled by 'nLatent'."
    )
  }

  if (modelType == "irt" &&
      "thetaGrid" %in% names(dots) &&
      length(nLatent) > 1L) {
    stop(
      "'thetaGrid' can only be used with a single value of 'nLatent', ",
      "because the grid dimension must match the number of latent variables."
    )
  }

  # (2) Fit models

  modelList <- vector("list", length(nLatent))
  names(modelList) <- as.character(nLatent)

  tabSearch <- data.frame(
    nLatent = nLatent,
    aic = NA_real_,
    bic = NA_real_,
    runTime = NA_real_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(nLatent)) {

    k <- nLatent[i]

    time1 <- Sys.time()

    if (modelType == "irt") {

      modelFit <- do.call(
        fitIrt,
        c(
          list(
            items = items,
            formula = formula,
            data = data,
            model = k
          ),
          dots
        )
      )

      aicValue <- as.numeric(mirt::extract.mirt(modelFit$fit, "AIC"))
      bicValue <- as.numeric(mirt::extract.mirt(modelFit$fit, "BIC"))

    } else if (modelType == "lc") {

      modelFit <- do.call(
        fitLc,
        c(
          list(
            items = items,
            formula = formula,
            data = data,
            nClasses = k
          ),
          dots
        )
      )

      aicValue <- as.numeric(modelFit$fit$aic)
      bicValue <- as.numeric(modelFit$fit$bic)
    }

    runTime <- difftime(Sys.time(), time1, units = "secs")[[1]]

    tabSearch$aic[i] <- aicValue
    tabSearch$bic[i] <- bicValue
    tabSearch$runTime[i] <- runTime

    modelList[[as.character(k)]] <- modelFit
  }

  # (3) Select best model

  critValues <- tabSearch[[criterion]]

  if (all(is.na(critValues))) {
    stop("All fitted models have missing ", toupper(criterion), " values.")
  }

  bestId <- which(critValues <= min(critValues, na.rm = TRUE))
  bestId <- bestId[1L]

  bestNLatent <- tabSearch$nLatent[bestId]

  tabSearch$selected <- FALSE
  tabSearch$selected[bestId] <- TRUE

  bestModel <- modelList[[as.character(bestNLatent)]]

  # (4) Add search output to selected model

  bestModel$search <- list(
    table = tabSearch,
    models = modelList,
    criterion = criterion,
    bestNLatent = bestNLatent,
    bestValue = tabSearch[[criterion]][bestId],
    modelType = modelType
  )

  return(bestModel)
}

#' Select best model from a latent-model search object
#'
#' @description
#' \code{selectBestSearchModel()} re-selects the best model from an object
#' returned by  \code{\link{searchLatent}}, \code{\link{fitIrtSearchLatent}},
#' or \code{\link{fitLcSearchLatent}}. The function does not refit any models.
#' It uses the stored AIC/BIC table and the stored fitted models in the
#' \code{search} component.
#'
#' @param modelSearch Model object returned by \code{\link{searchLatent}},
#'   \code{\link{fitIrtSearchLatent}}, or \code{\link{fitLcSearchLatent}}.
#'   The object must contain a \code{search} component.
#' @param aic Logical; if \code{TRUE}, the model with the lowest AIC is selected.
#'   If \code{FALSE}, the model with the lowest BIC is selected.
#'
#' @return The selected model object. The returned object has the same structure
#'   as the  \code{modelSearch} object, with an updated \code{search} component. The
#'   \code{search$table} component contains an updated \code{selected} column,
#'   and \code{search$criterion}, \code{search$bestNLatent}, and
#'   \code{search$bestValue} are updated according to the chosen criterion.
#'
#' @export
#' @examples # no example so far
selectBestSearchModel <- function(modelSearch,
                                  aic = FALSE) {

  # (1) Checks

  if (!is.list(modelSearch)) {
    stop("'modelSearch' must be a model object returned by searchLatent().")
  }

  if (is.null(modelSearch$search)) {
    stop("'modelSearch' must contain a 'search' component.")
  }

  if (is.null(modelSearch$search$table)) {
    stop("'modelSearch$search$table' is missing.")
  }

  if (is.null(modelSearch$search$models)) {
    stop("'modelSearch$search$models' is missing.")
  }

  if (!is.logical(aic) || length(aic) != 1L || is.na(aic)) {
    stop("'aic' must be TRUE or FALSE.")
  }

  tabSearch <- modelSearch$search$table
  modelList <- modelSearch$search$models

  requiredCols <- c("nLatent", "aic", "bic")

  if (!all(requiredCols %in% names(tabSearch))) {
    stop(
      "'modelSearch$search$table' must contain the columns: ",
      paste(requiredCols, collapse = ", "), "."
    )
  }

  if (!is.list(modelList) || length(modelList) == 0L) {
    stop("'modelSearch$search$models' must be a non-empty list.")
  }

  criterion <- if (isTRUE(aic)) "aic" else "bic"


  # (2) Select best model

  critValues <- tabSearch[[criterion]]

  bestId <- which(critValues <= min(critValues, na.rm = TRUE))
  bestId <- bestId[1L]

  bestNLatent <- tabSearch$nLatent[bestId]

  tabSearch$selected <- FALSE
  tabSearch$selected[bestId] <- TRUE


  # (3) Extract selected model

  modelName <- as.character(bestNLatent)

  if (modelName %in% names(modelList)) {
    bestModel <- modelList[[modelName]]
  } else if (length(modelList) >= bestId) {
    bestModel <- modelList[[bestId]]
  } else {
    stop("Could not find the selected model in 'modelSearch$search$models'.")
  }


  # (4) Avoid recursive search objects

  modelList <- lapply(modelList, function(x) {
    x$search <- NULL
    x
  })

  bestModel$search <- modelSearch$search
  bestModel$search$table <- tabSearch
  bestModel$search$models <- modelList
  bestModel$search$criterion <- criterion
  bestModel$search$bestNLatent <- bestNLatent
  bestModel$search$bestValue <- tabSearch[[criterion]][bestId]

  return(bestModel)
}

#' Compute exact full joint response distribution from an latent class model (helper function)
#'
#' @description
#' Function used by predJointDistRespLc() when fullJoint = TRUE.
#' It enumerates all possible response patterns and computes their exact
#' probabilities under the latent class (LC) model. The computation is based on
#' publicly available R code (XX) from the study of XX.
#'
#' @param classProb Numeric vector of class probabilities. These are either
#' prior class probabilities P(C = k) or posterior class probabilities
#' P(C = k | observed responses) if responses have already been conditioned on.
#' @param itemProbs List of item probability matrices. Each element corresponds
#' to one item and has one row per latent class and one column per response
#' category.
#' @param varLabels List of original item response labels.
#' @param items Character vector of item names.
#' @param givenValAll Optional named vector of already observed item responses.
#'
#' @return Data frame with columns item1, ..., itemJ, freq.
#'
#' @noRd
fullJointDistRespLc <- function(classProb,
                                itemProbs,
                                varLabels,
                                items,
                                givenValAll = NULL) {

  nClasses <- length(classProb)

  if (any(!is.finite(classProb)) ||
      any(classProb < 0) ||
      sum(classProb) <= 0) {
    stop("'classProb' must contain valid nonnegative class probabilities.")
  }

  classProb <- classProb / sum(classProb)

  if (!is.null(givenValAll)) {
    if (is.null(names(givenValAll)) ||
        !all(names(givenValAll) %in% items)) {
      stop("Names of 'givenValAll' must match item names.")
    }
  }

  givenItems <- if (is.null(givenValAll)) character(0) else names(givenValAll)
  unknownItems <- setdiff(items, givenItems)

  # Enumerate response patterns for unknown items only.
  # If no responses are known, this is the full joint distribution.
  # If responses are known, this is the full conditional distribution
  # over the remaining items.
  if (length(unknownItems) == 0L) {

    patternUnknown <- data.frame(.dummy = 1L)
    patternUnknown <- patternUnknown[, 0, drop = FALSE]

  } else {

    # Number of possible patterns
    nPatterns <- prod(vapply(varLabels[unknownItems], length, numeric(1)))

    patternUnknown <- expand.grid(
      varLabels[unknownItems],
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )

    names(patternUnknown) <- unknownItems
  }


  # Insert known and unknown responses into the original item order.
  patternAll <- data.frame(
    matrix(NA, nrow = nrow(patternUnknown), ncol = length(items)),
    stringsAsFactors = FALSE
  )

  names(patternAll) <- items

  for (j in items) {

    if (j %in% givenItems) {
      patternAll[[j]] <- givenValAll[[j]]
    } else {
      patternAll[[j]] <- patternUnknown[[j]]
    }
  }

  # Compute exact response-pattern probabilities.
  # If givenValAll is supplied, classProb is already P(C | Y_observed),
  # so we multiply only over the unknown items.
  freq <- numeric(nrow(patternAll))

  for (k in seq_len(nClasses)) {

    prob_k <- rep(classProb[k], nrow(patternAll))

    for (j in unknownItems) {

      probs <- as.matrix(itemProbs[[j]])
      varLabels_j <- varLabels[[j]]

      # Ensure columns correspond to stored response labels.
      if (!is.null(colnames(probs)) &&
          all(as.character(varLabels_j) %in% colnames(probs))) {
        probs <- probs[, as.character(varLabels_j), drop = FALSE]
      }

      # Match response values to item-probability columns.
      # First try exact character matching.
      respCol <- match(
        as.character(patternAll[[j]]),
        as.character(varLabels_j)
      )

      prob_k <- prob_k * probs[k, respCol]
    }

    freq <- freq + prob_k
  }

  jointDist <- patternAll
  jointDist$freq <- freq

  jointDist <- jointDist[jointDist$freq > 0, , drop = FALSE]

  if (sum(jointDist$freq) <= 0) {
    stop("The computed full joint distribution has total probability 0.")
  }

  jointDist$freq <- jointDist$freq / sum(jointDist$freq)

  # Make item values.
  facToNumeric <- function(x) as.numeric(as.character(x))

  jointDist[, items] <- lapply(
    jointDist[, items, drop = FALSE],
    facToNumeric
  )

  rownames(jointDist) <- NULL

  return(jointDist)
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
