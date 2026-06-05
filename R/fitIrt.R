#' Fit multidimensional graded item response theory models for the use with \code{dedaptive}
#'
#' @description
#' The function \code{fitIrt()} fits Multidimensional Item Response Theory (MIRT)
#' models for graded (ordinal) responses and returns a model object that can be used
#' for predictions and item selection within the \code{dedaptive} library.
#'
#' Technically, \code{fitIrt()} is a wrapper around \code{\link[mirt]{mirt}}:
#' it prepares the data, sets up an optional latent regression, and stores some
#' additional information (e.g., item labels) needed later by functions from \code{dedaptive}.
#' If you plan to use an MIRT model with \code{dedaptive}, you must always fit
#' it via \code{fitIrt()} rather than calling \code{\link[mirt]{mirt}} directly.
#'
#' @details
#' The function expects a set of item responses and, optionally, predictors for a
#' latent regression (e.g., age, sex). The names of the item columns are given
#' in \code{items}, and the predictors are specified via \code{formula}. All
#' predictor variables referenced in \code{formula} must be available as columns in \code{data}.
#' Additional arguments are passed to \code{\link[mirt]{mirt}} via \code{...}.
#'
#' Via the argument \code{model}, we can specify the latent structure, e.g., the
#' number of latent variables, which items load on which latent variables, and
#' whether the latent variables are correlated. The \code{model} argument is
#' passed directly to \code{\link[mirt]{mirt}}, so any model specification supported
#' by \code{\link[mirt]{mirt}} can be used. For a full description of possible model
#' specifications, see \code{\link[mirt]{mirt}}.
#'
#' Via the argument \code{method}, the estimation method can be specified through
#' \code{...} (see the documentation of \code{\link[mirt]{mirt}}).
#'
#' Independently of the estimation method, \code{fitIrt()} stores a prediction
#' grid for the latent variables. The grid is selected in the following order:
#' a user-supplied \code{thetaGrid}, a regular grid defined by \code{thetaLim}
#' or \code{thetaQuadpts}, the internal theta grid from the fitted
#' \code{\link[mirt]{mirt}} object when available, and finally a default regular
#' grid. This prediction grid is used in other functions of \code{dedaptive}
#' to approximate posterior distributions of the latent variables.
#'
#' @param items Character vector with the names of the columns in \code{data} containing
#' the item responses. These columns are treated as ordered responses and used to
#' fit the graded MIRT model.
#' @param formula Either \code{NULL} (no latent regression), a character string
#'  containing only the right-hand side of a regression formula
#'  (e.g., \code{"age + sex"}), or a one-sided formula (e.g., \code{~ age + sex})
#'  specifying the predictors for the latent regression.
#' @param data A data frame containing the item responses specified in
#'  \code{items} and, if \code{formula} is not \code{NULL}, all predictor
#'  variables referenced in \code{formula}. Each row typically corresponds to
#'  one person.
#' @param thetaLim Optional numeric vector of length 2 defining the lower and
#'   upper limits of the prediction grid for the latent variables. If
#'   \code{NULL}, no user-defined grid limits are used. In that case,
#'   \code{fitIrt()} uses the internal theta grid from the fitted
#'   \code{\link[mirt]{mirt}} object when available; otherwise, a default
#'   regular prediction grid is created.
#' @param thetaQuadpts Optional integer specifying the number of grid points per
#'   latent dimension used for the prediction grid. If \code{NULL}, no
#'   user-defined number of grid points is used. If a new regular grid is
#'   created, default values follow the usual \code{mirt} quadrature scheme
#'   depending on the number of latent factors.
#' @param thetaGrid Optional numeric matrix containing a custom prediction grid
#'   for the latent variables. The matrix must have one column per latent factor.
#'   If supplied, \code{thetaLim} and \code{thetaQuadpts} are ignored.
#' @param ... Additional arguments passed to \code{\link[mirt]{mirt}}, such as \code{model}
#' (latent structure passed to \code{\link[mirt]{mirt}} e.g., an object created by
#' \code{\link[mirt]{mirt.model}} or an integer specifying the dimension of the latent space),
#' \code{method} (default \code{method = "EM"}), \code{technical} options, starting values,
#' or convergence settings.
#'
#' @return
#' A list with the following elements:
#' \describe{
#'   \item{\code{modelType}}{Character specfying the used model type ("irt", meta-data)}
#'
#'   \item{\code{items}}{Character vector with the item names used in the model (meta-data).}
#'
#'   \item{\code{formula}}{Original \code{formula} argument as supplied by the user (meta-data).
#'     This is \code{NULL} if no latent regression was specified.}
#'
#'   \item{\code{covFormula}}{Parsed one-sided formula used internally for the
#'     latent regression. This is \code{NULL} if no latent regression was specified.}
#'
#'   \item{\code{thetaLim}}{User-supplied lower and upper limits of the prediction
#'     grid for the latent variables, or \code{NULL} if no limits were supplied.}
#'
#'   \item{\code{thetaQuadpts}}{User-supplied number of grid points per latent
#'     dimension, or \code{NULL} if no value was supplied.}
#'
#'   \item{\code{varLabels}}{List of length \code{length(items)}, where each
#'     element contains the sorted unique response categories for the corresponding
#'     item.}
#'
#'   \item{\code{fit}}{The fitted \code{\link[mirt]{mirt}} model object returned by
#'     \code{\link[mirt]{mirt}}.}
#'
#'   \item{\code{factorNames}}{Character vector with the names of the latent
#'     variables in the fitted model.}
#'
#'   \item{\code{nFactors}}{Number of latent variables in the fitted model.}
#'
#'   \item{\code{coef}}{List of estimated model parameters from the fitted
#'     \code{\link[mirt]{mirt}} object:
#'     \describe{
#'       \item{\code{paramItems}}{Estimated item parameters.}
#'       \item{\code{slopesItems}}{Estimated item slope parameters.}
#'       \item{\code{intItems}}{Estimated item intercept or threshold parameters.}
#'       \item{\code{paramReg}}{Estimated latent-regression coefficients, or \code{NULL}
#'         if no latent regression was fitted.}
#'       \item{\code{thetaCovPrior}}{Estimated covariance matrix of the latent
#'         variables.}
#'     }}
#'
#'   \item{\code{thetaGridPred}}{Prediction grid for the latent variables used by
#'     \code{dedaptive} functions to approximate posterior distributions of the
#'     latent variables conditional on observed item responses.}
#'
#'   \item{\code{itemProbs}}{List of item category response probabilities evaluated
#'     on \code{thetaGridPred}. Each list element corresponds to one item and
#'     contains a matrix with one row per grid point and one column per response
#'     category.}
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
                   thetaLim = NULL,
                   thetaQuadpts = NULL,
                   thetaGrid = NULL,
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

  # Initialize output and some meta-data
  modelOut <- list()
  modelOut$modelType <- "irt"
  modelOut$items <- items
  modelOut["formula"] <- list(formula)
  modelOut$thetaLim <- thetaLim
  modelOut$thetaQuadpts <- thetaQuadpts

  # Labels of item categories
  varLabels <- lapply(
    data[, items, drop = FALSE],
    function(x) sort(unique(x[!is.na(x)]))
  )
  modelOut$varLabels <- varLabels

  # (2) Prepare latent regression
  if (is.null(formula)) {
    covFormula <- NULL
    dataReg <- NULL
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

  # Add formula to the output
  modelOut["covFormula"] <- list(covFormula)

  # (3) Fit MIRT model and extract quantitites needed for predictions
  fit <- mirt::mirt(
    data    = data[, items, drop = FALSE],
    formula = covFormula,
    covdata = dataReg,
    itemtype = "graded",
    ...
  )
  modelOut$fit <- fit

  # number and names of factors
  factorNames <- mirt::extract.mirt(fit, "factorNames")
  nFactors <- length(factorNames)

  modelOut$factorNames <- factorNames
  modelOut$nFactors <- nFactors

  # estimated model parameters
  coefFit <- mirt::coef(fit, simplify = TRUE)

  ## item parameters
  paramItems <- coefFit$items
  slopesItems <- paramItems[, grepl("^a", colnames(paramItems)), drop = FALSE]
  intItems <- paramItems[, grepl("^d", colnames(paramItems)), drop = FALSE]

  ## regression coefficients
  paramReg <- if (!is.null(coefFit$lr.betas)) coefFit$lr.betas else NULL

  ## covariance matrix latent variables (prior distribution)
  thetaCovPrior <- coefFit$cov

  ## Add to output
  modelOut$coef <- list(paramItems = paramItems,
                        slopesItems = slopesItems,
                        intItems = intItems,
                        paramReg = paramReg,
                        thetaCovPrior = thetaCovPrior)

  # more information about the items
  itemObjects <- fit@ParObjects$pars[seq_len(fit@Data$nitems)]

  # theta grid that is used for predictions (in other functions from dedaptive)
  thetaGridPred <- getThetaGridPred(
    fit = fit,
    nFactors = nFactors,
    factorNames = factorNames,
    thetaLim = thetaLim,
    thetaQuadpts = thetaQuadpts,
    thetaGrid = thetaGrid
  )

  modelOut$thetaGridPred <- thetaGridPred

  # Compute item category response probabilities on the prediction grid
  itemProbs <- lapply(itemObjects, function(item) {
    mirt::probtrace(item, thetaGridPred)
  })
  names(itemProbs) <- items

  modelOut$itemProbs <- itemProbs

  return(modelOut)
}
