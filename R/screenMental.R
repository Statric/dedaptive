#' Simulated data set for personalized mental health screening with dedaptive testing
#'
#' @description
#' \code{screenMental} is a simulated data set based on statistical methods applied to
#' the data set used in the publication introducing the dedaptive testing framework
#' (Wyss et al., 2025). It can be used to demonstrate the functions \code{\link{fitIrt}},
#' \code{\link{predJointDistRespIrt}}, \code{\link{dedaptiveIrt}}, and
#' \code{\link{fixSelectionIrt}}.
#'
#' @details
#' The original data stems from a study investigating internet-based cognitive
#' behavioral therapy for social anxiety (Šipka et al., 2025). A multidimensional
#' IRT model was trained on the nine items of the Patient Health Questionnaire-9
#' (PHQ-9) and the seven items of the Generalized Anxiety Disorder-7 (GAD-7)
#' scale. The model assumed two latent variables (all items loading on both
#' latent variables) and a latent regression with effects of sex and age on the
#' means of the two latent variables (see Wyss et al., 2025).
#'
#' The dataset was simulated as follows:
#' \enumerate{
#'   \item Training on the original data (652 persons):
#'     \enumerate{
#'       \item Estimate the relative frequencies of male and female participants.
#'       \item Fit kernel density estimates for the age distribution separately
#'         for male and female individuals.
#'       \item Fit a multidimensional IRT model on PHQ-9 and GAD-7 item responses
#'         using \code{\link{fitIrt}}.
#'     }
#'   \item Simulation of a synthetic dataset of 652 persons:
#'     \enumerate{
#'       \item Simulate sex according to the estimated frequencies from step 1(a).
#'       \item For each simulated sex, draw ages from the corresponding
#'         kernel density estimates from step 1(b).
#'       \item Given sex and age, simulate PHQ-9 and GAD-7 item responses from
#'         sex- and age-specific joint distributions using the model from step 1(c)
#'         and \code{\link{predJointDistRespIrt}}.
#'     }
#' }
#'
#' All PHQ-9 and GAD-7 items are ordinal with possible values 0, 1, 2, and 3,
#' corresponding to increasing symptom frequency. English item labels and
#' response categories can be found in the official PHQ-9 and GAD-7 forms.
#'
#' @format
#' A data frame with 652 rows (persons) and 19 variables:
#' \describe{
#'   \item{\code{id}}{Integer person identifier.}
#'   \item{\code{sex}}{Sex (numeric; 0 = male, 1 = female).}
#'   \item{\code{age}}{Age in years (numeric).}
#'   \item{\code{phq1}}{PHQ-9 item 1 (loss of interest / pleasure).}
#'   \item{\code{phq2}}{PHQ-9 item 2 (depressed mood).}
#'   \item{\code{phq3}}{PHQ-9 item 3 (sleep problems).}
#'   \item{\code{phq4}}{PHQ-9 item 4 (fatigue / low energy).}
#'   \item{\code{phq5}}{PHQ-9 item 5 (changes in appetite).}
#'   \item{\code{phq6}}{PHQ-9 item 6 (feelings of worthlessness or failure).}
#'   \item{\code{phq7}}{PHQ-9 item 7 (concentration problems).}
#'   \item{\code{phq8}}{PHQ-9 item 8 (psychomotor changes: slowed or restless).}
#'   \item{\code{phq9}}{PHQ-9 item 9 (suicidal / self-harm thoughts).}
#'   \item{\code{gad1}}{GAD-7 item 1 (nervousness / anxiety).}
#'   \item{\code{gad2}}{GAD-7 item 2 (uncontrollable worrying).}
#'   \item{\code{gad3}}{GAD-7 item 3 (excessive worry about different things).}
#'   \item{\code{gad4}}{GAD-7 item 4 (difficulty relaxing).}
#'   \item{\code{gad5}}{GAD-7 item 5 (restlessness / difficulty sitting still).}
#'   \item{\code{gad6}}{GAD-7 item 6 (irritability / becoming easily annoyed).}
#'   \item{\code{gad7}}{GAD-7 item 7 (fear that something awful might happen).}
#' }
#'
#' @usage data(screenMental)
#'
#' @references
#' Šipka, D., Lopes, R., Krieger, T., Klein, J. P., & Berger, T. (2025).
#' Active components in internet-based cognitive-behavioral therapy for social anxiety
#' disorder: a randomized full factorial trial. \emph{Psychotherapy and Psychosomatics},
#' 94(1), 40–59.
#'
#' Wyss, P., Steiner, M., Lopes, R., Šipka, D., Berger, T., & Krause, A. (2025).
#' Decision-oriented adaptive testing for efficient screening across mental disorders.
#' Manuscript in preparation.
#'
#' @examples
#' data(screenMental)
#' str(screenMental)
"screenMental"
