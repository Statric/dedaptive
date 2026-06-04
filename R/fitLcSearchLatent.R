#' Search latent class models over varying numbers of latent classes
#'
#' \code{fitLcSearchLatent()} fits several Latent Class (LC) models
#' with different numbers of latent classes and selects the best model according to AIC or BIC.
#' The selected model is returned in the same format as \code{\link{fitLc}},
#' with an additional \code{search} component containing all fitted models and
#' the model-selection table. The function is based on the helper function \code{searchLatent}.
#'
#' @param items Character vector with the names of the columns in \code{data} containing
#' the item responses. These columns are treated as ordered responses and used to
#' fit the multidimensional graded IRT model.
#' @param formula Either \code{NULL} (no latent class regression), a character string
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
#' @param ... Additional arguments passed to \code{\link{fitLc}}.
#'
#' @return A list object with the same structure as returned by
#'   \code{\link{fitLc}}, based on the selected model. In addition, the object
#'   contains a \code{search} component with:
#'   \describe{
#'     \item{\code{table}}{Data frame with number of latent variables, AIC, BIC,
#'       runtime, and selected-model indicator.}
#'     \item{\code{models}}{List of all fitted models.}
#'     \item{\code{criterion}}{Selection criterion, either \code{"aic"} or
#'       \code{"bic"}.}
#'     \item{\code{bestNLatent}}{Number of latent variables of the selected
#'       model.}
#'   }
#' @examples
#' \dontrun{
#' # Will follow
#' }
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
