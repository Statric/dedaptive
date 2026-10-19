#' Search latent class models over varying numbers of latent classes
#'
#' \code{fitLcSearchLatent()} fits several Latent Class (LC) models with
#' \code{\link{fitLc}} (based on the package \pkg{poLCA}; Linzer & Lewis, 2011)
#' with different numbers of latent classes and selects the best model according
#' to the Akaike Information Criterion (AIC) or the Bayesian Information Criterion
#' (BIC). Selecting the number of latent classes of an unrestricted LC model by an
#' information criterion is inspired by the publicly available R code of
#' Psychogyiopoulos et al. (2025; \url{https://osf.io/g5hz7/}).The selected model is returned in the same format as \code{\link{fitLc}},
#' with an additional \code{search} component containing all fitted models and
#' the model-selection table. The function is based on the helper function
#' \code{\link{searchLatent}}.
#'
#' @param items Character vector with the names of the columns in \code{data} containing
#' the item responses. These columns are used to fit the LC model.
#' @param formula Either \code{NULL} (no latent class regression), a character string
#'  containing only the right-hand side of a regression formula
#'  (e.g., \code{"age + sex"}), or a one-sided formula (e.g., \code{~ age + sex})
#'  specifying the predictors for the latent class regression.
#' @param data A data frame containing the item responses specified in
#'  \code{items} and, if \code{formula} is not \code{NULL}, all predictor
#'  variables referenced in \code{formula}. Each row typically corresponds to
#'  one person.
#' @param nLatent Integer vector with the numbers of latent classes to search
#'   over.
#' @param aic Logical; if \code{TRUE}, the model with the lowest AIC is selected.
#'   If \code{FALSE}, the model with the lowest BIC is selected.
#' @param ... Additional arguments passed to \code{\link{fitLc}}.
#'
#' @return A list object with the same structure as returned by
#'   \code{\link{fitLc}}, based on the selected model. In addition, the object
#'   contains a \code{search} component with:
#'   \describe{
#'     \item{\code{table}}{Data frame with number of latent classes, AIC, BIC,
#'       runtime, and selected-model indicator.}
#'     \item{\code{models}}{List of all fitted models. Each model contains its
#'       \code{fit} object with the data used for fitting.}
#'     \item{\code{criterion}}{Selection criterion, either \code{"aic"} or
#'       \code{"bic"}.}
#'     \item{\code{bestNLatent}}{Number of latent classes of the selected
#'       model.}
#'   }
#'
#' @references
#' Linzer, D. A., & Lewis, J. B. (2011). poLCA: An R package for polytomous
#' variable latent class analysis. \emph{Journal of Statistical Software},
#' \emph{42}(10), 1–29. \doi{10.18637/jss.v042.i10}
#'
#'
#' @examples
#' data(screenMental)
#' data(modelScreenMental)
#'
#' # Standardize age as in the model (mean and SD of the original data)
#' ageStandParam <- modelScreenMental$ageStandParam
#' screenMental$ageStand <- (screenMental$age - ageStandParam[["mean"]]) / ageStandParam[["sd"]]
#'
#' items <- modelScreenMental$items
#'
#' # Select between two and three latent classes (BIC)
#' model <- fitLcSearchLatent(items = items, formula = "sex + ageStand",
#'                            data = screenMental, nLatent = 2:3,
#'                            nrep = 10, verbose = FALSE)
#' model$search$table
#'
#' @export

fitLcSearchLatent <- function(items,
                              formula = NULL,
                              data,
                              nLatent = 1:5,
                              aic = FALSE,
                              ...) {

  searchLatent(
    items = items,
    formula = formula,
    data = data,
    nLatent = nLatent,
    aic = aic,
    modelType = "lc",
    ...
  )
}
