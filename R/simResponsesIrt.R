#' Simulate item responses from a fitted Item Response Theory model
#'
#' @description
#' \code{simResponsesirt} simulates item response patterns from an item Response
#'  Theory (IRT) model fitted with \code{\link{fitIrt}}, using the probabilistic
#'  machinery of \code{\link{predJointDistRespIrt}}.
#'
#' @details
#' If the IRT model includes a latent regression (\code{model$formula} not \code{NULL}),
#' \code{dataPred} must contain all predictor variables used in \code{\link{fitIrt}}.
#' For each row in \code{dataPred}, one response pattern is simulated.
#'
#' If the model has no latent regression (\code{model$formula} is \code{NULL}) and
#' \code{dataPred} is \code{NULL}, then \code{nSim} must be supplied and \code{nSim}
#' response patterns are simulated.
#'
#' @param model IRT model fitted with \code{\link{fitIrt}}.
#' @param dataPred Optional data frame with predictors for latent regression.
#' @param nSim Number of simulated response patterns when \code{dataPred} is \code{NULL}
#' and no regression is used.
#' @param seed Integer seed for reproducibility.
#'
#' @return A data frame with predictors and simulated responses.
#' @import mirt
#' @export
simResponsesIrt <- function(model,
                            dataPred = NULL,
                            nSim = NULL,
                            seed = 1) {

  # 1) Prepare

  # Create new ID
  if (is.null(dataPred)) {
    dataPred <- data.frame(idSim = seq_len(nSim))
  } else {
    dataPred <- as.data.frame(dataPred)
    dataPred$idSim <- seq_len(nrow(dataPred))
  }

  nRows <- nrow(dataPred)

  # Row-specific seeds
  set.seed(seed)
  rowSeeds <- sample.int(.Machine$integer.max, size = nRows, replace = FALSE)

  # 2) Helper function for simulation per row

  simOneRow <- function(i) {
    dataSub <- dataPred[i, , drop = FALSE]
    thisSeed <- as.integer(rowSeeds[i])

    simOut <- predJointDistRespIrt(
      model     = model,
      dataSub   = dataSub,
      nSimTheta = 1,
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
  respMat$idSim<- NULL
  dataPred$idSim<- NULL

  # 4) Combine predictors and simulated responses
  dataOut <- cbind(dataPred, respMat)

  return(dataOut)
}
