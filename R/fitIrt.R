
#' Fit multidimensional graded IRT models for use with \code{dedaptive}
#'
#' @description
#' The function \code{fitIrt()} fits multidimensional graded IRT models and
#' returns a model object that can be used for predictions and item selection
#' within the \code{dedaptive} library.
#'
#' Technically, \code{fitIrt()} is a thin wrapper around \code{mirt::mirt()}:
#' it prepares the data, sets up an optional latent regression, and stores some
#' additional information (e.g., item labels) needed later by \code{dedaptive}.
#' If you plan to use an IRT model with \code{dedaptive}, you must always fit
#' it via \code{fitIrt()} rather than calling \code{mirt::mirt()} directly.
#'
#' @details
#' The function expects a set of item responses and, optionally, predictors for a
#' latent regression (e.g., age, sex). The names of the item columns are given
#' in \code{respName}, and the predictors are specified via \code{formula}.
#'
#' The argument \code{formula} can be supplied either as
#' \itemize{
#'   \item a **character string** containing only the right-hand side (RHS) of a
#'     regression formula (e.g., \code{"age + sex"}), or
#'   \item a **one-sided formula** (e.g., \code{~ age + sex}).
#' }
#' Internally, character input is converted to a one-sided formula (e.g.
#' \code{"age + sex"} becomes \code{~ age + sex}) and then passed to
#' \code{mirt} as a latent regression. All predictor variables referenced in
#' \code{formula} must be available as columns in \code{data}.
#'
#' Via the argument \code{model}, we can specify the latent structure, e.g., the
#' number of latent variables, which items load on which latent variables, and
#' whether the latent variables are correlated. The \code{model} argument is
#' passed directly to \code{mirt::mirt()}, so any model specification supported
#' by \code{mirt} can be used. For a full description of possible model
#' specifications, see \code{\link[mirt]{mirt}}.
#'
#' Currently, the IRT model is estimated using the Expectation-Maximization (EM) algorithm as implemented
#' in \code{mirt} (i.e., \code{method = "EM"} in the underlying call to
#' \code{mirt::mirt()}). In future versions, additional estimation methods
#' supported by \code{mirt} may be exposed via \code{fitIrt()}.
#'
#' @param respName Character vector with the names of the item/response columns
#'   in \code{data}. These columns are treated as ordered responses and used to
#'   fit the graded IRT model.
#' @param formula Either \code{NULL} (no latent regression), a character string
#'   containing only the right-hand side of a regression formula
#'   (e.g., \code{"age + sex"}), or a one-sided formula (e.g., \code{~ age + sex})
#'   specifying the predictors for the latent regression.
#' @param data A data frame containing the item responses specified in
#'   \code{respName} and, if \code{formula} is not \code{NULL}, all predictor
#'   variables referenced in \code{formula}. Each row typically corresponds to
#'   one person.
#' @param model Specification of the latent structure passed to \code{mirt::mirt()}.
#'   This can be a single integer (number of dimensions), a character string with
#'   \code{mirt} model syntax, or an object created by \code{mirt::mirt.model()}.
#'   By default, a unidimensional model (\code{model = 1}) is fitted.
#' @param ... Additional arguments passed to \code{mirt::mirt()}, such as \code{model},
#'   \code{technical} options, starting values, or convergence settings. The
#'   estimation algorithm is currently fixed to the EM algorithm within
#'   \code{fitIrt()}; other methods supported by \code{mirt} may be made
#'   available in future versions.
#'
#' @return
#' A list with the following elements:
#' \describe{
#'   \item{\code{respName}}{Meta-data (character vector with the item names used in the model).}
#'   \item{\code{formula}}{Meta-data (The original \code{formula} argument as supplied by the user=.}
#'   \item{\code{varLabels}}{MA list of length \code{length(respName)}, where each
#'     element contains the sorted unique response categories for the corresponding item.}
#'   \item{\code{fit}}{The fitted \code{mirt} model object returned by
#'     \code{mirt::mirt()}. This object is used by \code{dedaptive} for prediction
#'     and item selection.}
#' }
#'
#' @examples
#' \dontrun{
#' # Will follow
#' )
#' }


fitIrt <- function(respName, formula = NULL, data, ...) {

  modelOut <- list()
  modelOut$respName <- respName
  modelOut$formula  <- formula   # original input (string or formula or NULL)

  ## 1) Labels of item categories
  varLabels <- lapply(
    data[, respName, drop = FALSE],
    function(x) sort(unique(x))
  )
  modelOut$varLabels <- varLabels

  ## 2) Prepare latent regression
  if (is.null(formula)) {
    covFormula <- NULL
    dataReg    <- NULL
  } else {
    # accept both character and formula
    if (inherits(formula, "formula")) {
      covFormula <- formula
    } else {
      covFormula <- stats::as.formula(paste0("~", formula))
    }

    # extract predictor names
    varReg  <- all.vars(covFormula)
    # (if you prefer formula.tools: varReg <- formula.tools::get.vars(covFormula))
    dataReg <- data[, varReg, drop = FALSE]
  }

  ## 3) Fit IRT model
  modelOut$fit <- mirt::mirt(
    data    = data[, respName, drop = FALSE],
    formula = covFormula,
    covdata = dataReg,
    itemtype = "graded",
    method   = "EM",
    ...
  )

  return(modelOut)
}
