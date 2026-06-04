#' Fit latent class models for use with \code{dedaptive}
#'
#' @description
#' The function , \code{fitLatentClass()} fits an unrestricted Latent Class (LC) model
#' for categorical item responses and returns a model object that can be used for predictions
#' and item selection within \code{dedaptive} library.
#'
#' Technically, \code{fitLatentClass()} is a wrapper around \code{\link[poLCA]{poLCA}}:
#' it prepares the data, sets up an optional latent class regression, and stores
#' additional information (e.g., item labels) needed later by functions from \code{dedaptive}.
#' If you plan to use an LC model with \code{dedaptive}, you must always fit
#' it via \code{fitLatentClass()} rather than calling  \code{\link[poLCA]{poLCA}} directly.
#'
#' @details
#' The function expects a set of item responses and, optionally, predictors for
#' a latent class regression (e.g., age, sex). The names of the item columns are given
#' in \code{items}, and the predictors are specified via \code{formula}. All
#' predictor variables referenced in \code{formula} must be available as columns in \code{data}.
#' Additional arguments are passed to \code{\link[poLCA]{poLCA}} via \code{...}.
#'
#' @param items Character vector with the names of the columns in \code{data} containing
#' the item responses.
#' @param formula Either \code{NULL} (no latent regression), a character string
#'  containing only the right-hand side of a regression formula
#'  (e.g., \code{"age + sex"}), or a one-sided formula (e.g., \code{~ age + sex})
#'  specifying the predictors specifying predictors for latent class membership.
#' @param data A data frame containing the item responses specified in
#'  \code{items} and, if \code{formula} is not \code{NULL}, all predictor
#'  variables referenced in \code{formula}. Each row typically corresponds to
#'  one person.
#' @param nClasses An Integer specfying the number of latent classes.
#' @param ...  Additional arguments passed to \code{\link[poLCA]{poLCA}} such as
#' \code{nrep} (number of random starts), \code{maxiter} (Maximum number of EM iteration)
#' or \code{verbose}.
#'
#' @return
#' A list with the following elements:
#' \describe{
#'   \item{\code{modelType}}{Character specfying the used model type ("lc", meta-data)}
#'
#'   \item{\code{items}}{Character vector with the item names used in the model (meta-data).}
#'
#'   \item{\code{formula}}{Original \code{formula} argument as supplied by the user (meta-data).
#'     This is \code{NULL} if no latent class regression was specified.}
#'
#'   \item{\code{covFormula}}{Parsed formula used internally for the
#'     latent class regression. This is \code{NULL} if no latent regression was specified.}
#'
#'   \item{\code{nClasses}}{Number of latent classes.}
#'
#'   \item{\code{varLabels}}{List of length \code{length(items)}, where each
#'     element contains the sorted unique response categories for the corresponding
#'     item on the original scale.}
#'
#'   \item{\code{labelMap}}{List of length \code{length(items)}, where each
#'     element contains a mapping from the original item labels to the ones used
#'     in \code{\link[poLCA]{poLCA}}.}
#'
#'   \item{\code{fit}}{The fitted \code{\link[poLCA]{poLCA}} model object returned by
#'     \code{\link[poLCA]{poLCA}}.}
#'
#'   \item{\code{coef}}{List containing prior class-membership regression coefficients
#'   (\code{NULL} if \code{formula} is not \code{NULL}) and regression coefficients
#'   (\code{NULL} if \code{formula} is \code{NULL}).}
#'
#'   \item{\code{itemProbs}}{List of item category response probabilities.
#'     Each list element is a matrix with one row per latent class and one column
#'     per response category.}
#' }
#'
#' @examples
#' \dontrun{
#' # Will follow
#' }
#' @import poLCA
#' @importFrom stats as.formula
#' @export

fitLc<- function(items,
                  formula = NULL,
                  data,
                  nClasses,
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

  # if (anyNA(data[, items, drop = FALSE])) {
  #   stop("Item responses contain missing values. Handle missing values before fitting.")
  # }

  ## Number of latent classes
  if (!is.numeric(nClasses) || length(nClasses) != 1L || nClasses < 1) {
    stop("'nClasses' must be a single positive integer.")
  }

  nClasses <- as.integer(nClasses)

  # Initialize output and some meta-data
  modelOut <- list()
  modelOut$modelType <- "lc"
  modelOut$items <- items
  modelOut["formula"] <- list(formula)
  modelOut$nClasses <- nClasses

  # Labels of item categories on the original response scale
  varLabels <- lapply(
    data[, items, drop = FALSE],
    function(x) sort(unique(x[!is.na(x)]))
  )

  modelOut$varLabels <- varLabels

  # Recode item responses for poLCA
  # poLCA requires item categories coded as positive integers: 1, 2, ..., C.
  dataLca <- data

  labelMap <- vector("list", length(items))
  names(labelMap) <- items

  for (item in items) {
    labs <- varLabels[[item]]

    dataLca[[item]] <- match(data[[item]], labs)

    labelMap[[item]] <- data.frame(
      original = labs,
      lca = seq_along(labs)
    )
  }

  modelOut$labelMap <- labelMap

  # (2) Prepare latent class regression formula

  # left hand-side of formula
  lhs <- paste0("cbind(", paste(items, collapse = ", "), ")")

  if (is.null(formula)) {
    # right hand side of formula
    rhs <- "~ 1"

  } else if (inherits(formula, "formula")){
    # Accept one-sided formulas such as ~ age + sex.
    # as.character(~ age + sex) --> c("~", "age + sex")

    formChr <- as.character(formula)

    if (length(formChr) != 2L) {
      stop("'formula' must be NULL, a one-sided formula, or a single character string.")
    }

    rhs <- paste0("~ ", formChr[2L])

  } else if (is.character(formula) && length(formula) == 1L){

    rhs <- if (grepl("~", formula, fixed = TRUE)) formula else paste0("~", formula)

  } else {
    stop("'formula' must be NULL, a one-sided formula, or a single character string.")
  }

  covFormula <- stats::as.formula(
    paste0(lhs, rhs)
  )

  varReg <- all.vars(covFormula)
  if (!all(varReg %in% names(data))) {
    stop("All predictors in 'formula' must be columns in 'data'.")
  }

  modelOut["covFormula"] <- list(covFormula)

  # (3) Fit latent class model and extract quantities needed for predictions

  # Model training
  fit <- poLCA::poLCA(
    formula = covFormula,
    data = dataLca,
    nclass = nClasses,
    ...
  )

  modelOut$fit <- fit

  # estimated model parameters
  if (is.null(formula)) {
    ## Latent class priors
    classPrior <- fit$P

    ## regression coefficients
    paramReg <- NULL
  } else {
    ## Latent class
    classPrior <- NULL

    ## regression coefficients
    paramReg <- fit$coeff
  }

  ## Add to output
  modelOut$coef <- list(
    classPrior = classPrior,
    paramReg = paramReg
  )

  # Compute item category response probabilities for the latent classes ("grid")
  itemProbs <- fit$probs[items]

  for (i in items) {
    rownames(itemProbs[[i]]) <- paste0("class", seq_len(nClasses))
    colnames(itemProbs[[i]]) <- as.character(varLabels[[i]])
  }

  modelOut$itemProbs <- itemProbs

  return(modelOut)
}
