#' Search multidimensional item response theory models over varying numbers of latent variables
#'
#'#' \code{fitIrtSearchLatent()} fits several Item Response Theory (IRT) or Multidimensional
#' IRT (MIRT) models with \code{\link{fitIrt}} (based on the package \pkg{mirt};
#' Chalmers, 2012) with different numbers of latent variables and selects the
#' best model according to the Akaike Information Criterion (AIC) or the Bayesian
#' Information Criterion (BIC). The selected model is returned in the same format
#' as \code{\link{fitIrt}} with an additional \code{search} component containing
#' all fitted models and the model-selection table. The function is based on the
#' helper function \code{\link{searchLatent}}.
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
#' @param nLatent Integer vector with the numbers of latent variables to search
#'   over.
#' @param aic Logical; if \code{TRUE}, the model with the lowest AIC is selected.
#'   If \code{FALSE}, the model with the lowest BIC is selected.
#' @param ... Additional arguments passed to \code{\link{fitIrt}}.
#'
#' @return A list object with the same structure as returned by
#'   \code{\link{fitIrt}}, based on the selected model. In addition, the object
#'   contains a \code{search} component with:
#'   \describe{
#'     \item{\code{table}}{Data frame with number of latent variables, AIC, BIC,
#'       runtime, and selected-model indicator.}
#'     \item{\code{models}}{List of all fitted models. Each model contains its
#'       \code{fit} object with the data used for fitting.}
#'     \item{\code{criterion}}{Selection criterion, either \code{"aic"} or
#'       \code{"bic"}.}
#'     \item{\code{bestNLatent}}{Number of latent variables of the selected
#'       model.}
#'   }
#'
#' @references
#' Chalmers, R. P. (2012). mirt: A multidimensional item response theory
#' package for the R environment. \emph{Journal of Statistical Software},
#' \emph{48}(6), 1–29. \doi{10.18637/jss.v048.i06}
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
#' \donttest{
#' # Select between one and two latent variables (BIC)
#' model <- fitIrtSearchLatent(items = items, formula = "sex + ageStand",
#'                             data = screenMental, nLatent = 1:2)
#' model$search$table
#' }
#'
#' @export

fitIrtSearchLatent <- function(items,
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
    modelType = "irt",
    ...
  )
}
