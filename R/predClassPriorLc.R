#' Predict prior latent class probabilities for one observation
#'
#' @description
#' The function \code{predClassPriorLc()} computes the prior latent class probabilities for one
#' observation from a latent class model fitted with predClassPriorLc \code{fitLc()}.
#'
#' If the model was fitted without covariates, the function returns the fixed
#' class proportions. If the model was fitted with a latent class regression, 
#' the function returns subject-specific prior class probabilities using the 
#' multinomial-logit parameterization used by \code{\link[poLCA]{poLCA}} . Class 1 
#' is the reference class and its linear predictor is fixed to zero.
#'
#' @param model A latent class model fitted with \code{fitLc()}.
#' @param dataSub One-row data frame with predictor variables for one subject.
#' @return Numeric vector of prior latent class probabilities. The vector has
#'   length \code{model$nClasses} and names `class1`, `class2`, ...
#'
#' @import poLCA
#' @export

predClassPriorLc <- function(model,
                             dataSub) {
  
  nClasses <- model$nClasses
  
  if (is.null(model$formula)) {
    
    classPrior <- as.numeric(model$coef$classPrior)
    
    if (length(classPrior) != nClasses) {
      stop("'model$coef$classPrior' must have length equal to 'model$nClasses'.")
    }
    
  } else {
    
    # Remove the response from the poLCA formula.
    formReg <- stats::delete.response(stats::terms(model$covFormula))
    
    # Build model matrix.
    modelMat <- stats::model.matrix(formReg, dataSub)
    
    # Extract regression coefficients.
    # poLCA stores coefficients for classes 2, ..., K relative to class 1.
    paramReg <- as.matrix(model$coef$paramReg)
    
    if (ncol(paramReg) != nClasses - 1L) {
      stop(
        "'model$coef$paramReg' must have one column for each non-reference class."
      )
    }
    
    if (nrow(paramReg) != ncol(modelMat)) {
      stop(
        "The number of rows in 'model$coef$paramReg' must match the number of ",
        "columns in the latent class regression model matrix."
      )
    }
    
    # Align rows of paramReg with columns of modelMat if names are available.
    if (!is.null(rownames(paramReg)) &&
        all(colnames(modelMat) %in% rownames(paramReg))) {
      paramReg <- paramReg[colnames(modelMat), , drop = FALSE]
    }
    
    # Linear predictors.
    # Class 1 is the reference class, so its linear predictor is 0.
    linPred <- c(0, as.numeric(modelMat %*% paramReg))
    
    # Compute probabilities using numerically (numerically stable).
    expPred <- exp(linPred - max(linPred))
    classPrior <- expPred / sum(expPred)
  }
  
  if (any(!is.finite(classPrior)) || any(classPrior < 0)) {
    stop("Predicted class probabilities are invalid.")
  }
  
  if (sum(classPrior) <= 0) {
    stop("Predicted class probabilities sum to zero.")
  }
  
  classPrior <- classPrior / sum(classPrior)
  names(classPrior) <- paste0("class", seq_len(nClasses))
  
  return(classPrior)
}