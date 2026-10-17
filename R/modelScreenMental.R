#' Fitted MIRT model used to simulate the screenMental data set
#'
#' @description
#' \code{modelScreenMental} is the Multidimensional Item Response Theory (MIRT)
#' model that was fitted with \code{\link{fitIrt}} to the data set used in the
#' publication introducing the dedaptive testing framework (Wyss et al., 2026).
#' The original data stem from a study investigating internet-based cognitive
#' behavioral therapy for social anxiety (Šipka et al., 2025). The model was used
#' to simulate the item responses in \code{\link{screenMental}}. It can be used
#' directly with
#' \code{\link{predJointDistResp}}, \code{\link{fixSelection}},
#' \code{\link{dedaptive}} and \code{\link{simResponsesLatentModel}}.
#'
#' @details
#' The model has two latent variables (all PHQ-9 and GAD-7 items loading on
#' both) and a latent regression on sex and standardized age (\code{ageStand}).
#' The fitted \code{mirt} object (element \code{fit}) was removed because it
#' contains the original data. Information that is only available in this
#' object (e.g., AIC, BIC or standard errors) is therefore not included.
#'
#' Standardized age is computed as \code{(age - mean) / sd}, with the mean and
#' standard deviation of age in the original data stored in
#' \code{ageStandParam}.
#'
#' @format A list as returned by \code{\link{fitIrt}} without the element
#'   \code{fit}, and with the additional element
#'   \describe{
#'     \item{\code{ageStandParam}}{Named numeric vector with the mean and
#'       standard deviation of age in the original data.}
#'   }
#'
#' @references
#' Šipka, D., Lopes, R., Krieger, T., Klein, J. P., & Berger, T. (2025).
#' Active components in internet-based cognitive-behavioral therapy for social anxiety
#' disorder: a randomized full factorial trial. \emph{Psychotherapy and Psychosomatics},
#' 94(1), 40–59.
#'
#' Wyss, P., Steiner, M., Lopes, R., Šipka, D., Berger, T., & Krause, A. (2026).
#' Decision-oriented adaptive testing for efficient screening across mental disorders.
#' Manuscript in preparation.
#'
#' @seealso \code{\link{screenMental}}, \code{\link{fitIrt}}
#'
#' @examples
#' data(screenMental)
#' data(modelScreenMental)
#'
#' # Predicted joint distribution of the item responses for the first person
#' dataSub <- screenMental[1, ]
#' param <- modelScreenMental$ageStandParam
#' dataSub$ageStand <- (dataSub$age - param[["mean"]]) / param[["sd"]]
#' pred <- predJointDistResp(modelScreenMental, dataSub, nSimLatent = 200)
#' head(pred$jointDist)
"modelScreenMental"
