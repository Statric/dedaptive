
#' Fit multidimensional graded Item Response Theory models for the use with \code{dedaptive}
#'
#' @description
#' The function \code{fitIrt()} fits Multidimensional Item Response Theory (MIRT)
#' models for graded (ordinal) responses and returns a model object that can be used
#' for predictions and item selection within the \code{dedaptive} library.
#'
#' Technically, \code{fitIrt()} is a wrapper around \code{\link[mirt]{mirt}}:
#' it prepares the data, sets up an optional latent regression, and stores some
#' additional information (e.g., item labels) needed later by functions from \code{dedaptive}.
#' If you plan to use an IRT model with \code{dedaptive}, you must always fit
#' it via \code{fitIrt()} rather than calling \code{\link[mirt]{mirt}} directly.
#'
#' @details
#' The function expects a set of item responses and, optionally, predictors for a
#' latent regression (e.g., age, sex). The names of the item columns are given
#' in \code{items}, and the predictors are specified via \code{formula}. All
#' predictor variables referenced in \code{formula} must be available as columns in \code{data}.
#'
#' Via the argument \code{model}, we can specify the latent structure, e.g., the
#' number of latent variables, which items load on which latent variables, and
#' whether the latent variables are correlated. The \code{model} argument is
#' passed directly to \code{\link[mirt]{mirt}}, so any model specification supported
#' by \code{\link[mirt]{mirt}} can be used. For a full description of possible model
#' specifications, see \code{\link[mirt]{mirt}}.
#'
#' Currently, the IRT model is estimated using the Expectation-Maximization (EM) algorithm as implemented
#' in \code{\link[mirt]{mirt}} (i.e., \code{method = "EM"} in the underlying call to
#' \code{\link[mirt]{mirt}}). In future versions, additional estimation methods
#' supported by \code{\link[mirt]{mirt}} may be incorporated in \code{fitIrt()}.
#'
#' @param items Character vector with the names of the columns in \code{data} containing
#' the item responses. These columns are treated as ordered responses and used to
#' fit the multidimensional graded IRT model.
#' @param formula Either \code{NULL} (no latent regression), a character string
#'  containing only the right-hand side of a regression formula
#'  (e.g., \code{"age + sex"}), or a one-sided formula (e.g., \code{~ age + sex})
#'  specifying the predictors for the latent regression.
#' @param data A data frame containing the item responses specified in
#'  \code{items} and, if \code{formula} is not \code{NULL}, all predictor
#'  variables referenced in \code{formula}. Each row typically corresponds to
#'  one person.
#' @param ... Additional arguments passed to \code{\link[mirt]{mirt}}, such as \code{model}
#' (latent structure passed to \code{\link[mirt]{mirt}} e.g., an object created by
#' \code{\link[mirt]{mirt.model}} or an integer specifying the dimension of the latent space),
#' \code{technical} options, starting values, or convergence settings. The
#'  estimation algorithm is currently fixed to the EM algorithm within
#'  \code{fitIrt}; other methods supported by \code{\link[mirt]{mirt}} may be made
#'  available in future versions.
#'
#' @return
#' A list with the following elements:
#' \describe{
#'   \item{\code{items}}{Meta-data (character vector with the item names used in the model).}
#'   \item{\code{formula}}{Meta-data (the original \code{formula} argument as supplied by the user).}
#'   \item{\code{varLabels}}{List of length \code{length(items)}, where each
#'     element contains the sorted unique response categories for the corresponding item.}
#'   \item{\code{fit}}{The fitted \code{\link[mirt]{mirt}} model object returned by
#'     \code{\link[mirt]{mirt}}. This object is used by \code{dedaptive} for prediction
#'     and item selection.}
#' }
#'
#' @examples
#' \dontrun{
#' # Will follow
#' }
#' @import mirt
#' @importFrom stats as.formula
#' @export

fitIrt <- function(items,
                   formula = NULL,
                   data,
                   ...) {

  # (1) Preparation
  # Checks

  ## Items
  if (!is.character(items) || length(items) == 0L) {
    stop("'items' must be a non-empty character vector.")
  }
  if (!all(items %in% names(data))) {
    stop("All variables listed in 'items' must be columns in 'data'.")
  }

  ## Data
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }

  ## Method: Fixed to 'EM'
  dots <- list(...)
  if ("method" %in% names(dots)) {
    stop("Please do not supply 'method'; fitIrt() currently fixes method = 'EM'.")
  }

  # Initialize output and some meta-data
  modelOut <- list()
  modelOut$items <- items
  modelOut$formula  <- formula   # original input (string or formula or NULL)

  # Labels of item categories
  varLabels <- lapply(
    data[, items, drop = FALSE],
    function(x) sort(unique(x[!is.na(x)]))
  )
  modelOut$varLabels <- varLabels

  # (2) Prepare latent regression
  if (is.null(formula)) {
    covFormula <- NULL
    dataReg    <- NULL
  } else {
    # accept both character and formula
    if (inherits(formula, "formula")) {
      covFormula <- formula
    } else if (is.character(formula) && length(formula) == 1L) {
      covFormula <- stats::as.formula(
        if (grepl("~", formula, fixed = TRUE)) formula else paste0("~", formula)
      )
    } else {
      stop("'formula' must be NULL, a one-sided formula, or a single character string.")
    }

    # extract predictor names
    varReg <- all.vars(covFormula)
    ## Check if the predictor variables are contained in 'data'
    if (!all(varReg %in% names(data))) {
      stop("All predictors in 'formula' must be columns in 'data'.")
    }
    dataReg <- data[, varReg, drop = FALSE]
  }

  # (3) Fit MIRT model
  modelOut$fit <- mirt::mirt(
    data    = data[, items, drop = FALSE],
    formula = covFormula,
    covdata = dataReg,
    itemtype = "graded",
    method   = "EM",
    ...
  )

  return(modelOut)
}
