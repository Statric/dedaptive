#' Simulate item responses from a fitted latent variable model
#'
#' @description
#' \code{simResponsesLatentModel()} simulates item response patterns from a Multidimensional
#' Item Response Theory (MIRT) or Latent Class (LT) model fitted with
#' \code{\link{fitIrt}} respectively \code{\link{fitLc}}, using the
#' probabilistic machinery of \code{\link{predJointDistResp}}.
#'
#' @details
#' If the MIRT or LC model includes a latent regression (\code{model$formula} not \code{NULL}),
#' \code{dataPred} must contain all predictor variables used in \code{\link{fitIrt}}
#' respectively \code{\link{fitLc}}.For each row in \code{dataPred}, one response
#' pattern is simulated.
#'
#' If the model has no latent regression (\code{model$formula} is \code{NULL}) and
#' \code{dataPred} is \code{NULL}, then \code{nSim} must be supplied and \code{nSim}
#' response patterns are simulated.
#'
#' @param model MIRT model fitted with \code{\link{fitIrt}}.
#' @param dataPred Optional data frame with predictors for latent regression.
#' @param nSim Number of simulated response patterns when \code{dataPred} is
#'   \code{NULL} and no regression is used.
#' @param seed Integer seed for reproducibility.
#'
#' @return A data frame with predictors and simulated responses.
#' @import mirt
#' @export
simResponsesLatentModel <- function(model,
                                    dataPred = NULL,
                                    nSim = NULL,
                                    seed = 1) {

  # 1) Prepare
  if (!is.list(model) || is.null(model$items) || is.null(model$fit)) {
    stop("'model' must be an object returned by fitIrt().")
  }

  if (is.null(dataPred) && is.null(nSim)) {
    stop("If 'dataPred' is NULL, 'nSim' must be supplied.")
  }

  if (!is.null(nSim) && (!is.numeric(nSim) || length(nSim) != 1L || nSim < 1)) {
    stop("'nSim' must be a positive integer.")
  }

  # Create new ID
  if (is.null(dataPred)) {
    dataPred <- data.frame(idSim = seq_len(nSim))
  } else {
    dataPred <- as.data.frame(dataPred)
    dataPred$idSim <- seq_len(nrow(dataPred))
  }

  # Check if specified predictors in the formula are present
  if (!is.null(model$formula)) {
    if (is.null(dataPred) || nrow(dataPred) == 0) {
      stop("If the model includes a latent regression, 'dataPred' must contain the predictor variables.")
    }

    if (inherits(model$formula, "formula")) {
      formReg <- model$formula
    } else {
      formReg <- stats::as.formula(
        if (grepl("~", model$formula, fixed = TRUE)) model$formula else paste0("~", model$formula)
      )
    }

    varReg <- all.vars(formReg)

    if (!all(varReg %in% names(dataPred))) {
      stop("All latent regression predictors must be present in 'dataPred'.")
    }
  }

  nRows <- nrow(dataPred)
  if (nRows < 1) {
    stop("No rows available for simulation.")
  }

  # Row-specific seeds
  set.seed(seed)
  rowSeeds <- sample.int(.Machine$integer.max, size = nRows, replace = FALSE)

  # 2) Helper function for simulation per row

  simOneRow <- function(i) {
    dataSub <- dataPred[i, , drop = FALSE]
    thisSeed <- as.integer(rowSeeds[i])

    simOut <- predJointDistResp(
      model     = model,
      dataSub   = dataSub,
      nSimLatent = 1,
      nSimItem  = 1,
      seed      = thisSeed
    )

    simResp <- simOut$sim

    if (nrow(simResp) != 1L) {
      simResp <- simResp[1, , drop = FALSE]
    }

    return(simResp)
  }

  # 3) Generate a simulated data set

  # Simulate item responses
  listSim <- lapply(seq_len(nRows), simOneRow)
  respMat <- do.call(rbind, listSim)
  rownames(respMat) <- NULL
  respMat$idSim <- NULL
  dataPred$idSim <- NULL

  # 4) Combine predictors and simulated responses
  dataOut <- cbind(dataPred, respMat)

  return(dataOut)
}
