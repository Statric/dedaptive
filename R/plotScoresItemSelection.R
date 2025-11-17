#' Plot joint and marginal distributions of scores based on predictions of a subset of items
#'
#' @description
#' Creates a color-coded scatterplot (joint distribution) of two scores
#' (functions of item responses) together with plots of the marginal distributions
#' above and to the right. Thresholds are indicated by reference lines and
#' the probabilities are shown as well.
#'
#' The function assumes that \code{result} comes from \code{\link{dedaptiveIrt}}
#' or \code{\link{fixSelectionIrt}} and contains elements \code{distFun} and
#' \code{thres}. Via the argument \code{whichScores}, you can choose which two score
#' functions (and corresponding decisions/thresholds) to visualize; by default,
#' the first two scores are used.
#'
#' @param result List returned by \code{\link{dedaptiveIrt}} or
#'   \code{\link{fixSelectionIrt}}, containing at least \code{distFun} and
#'   \code{thres}.
#' @param whichScores Integer vector of length 2 indicating which two scores
#' to plot. For example, \code{c(1, 3)} plots \code{fun_1} against \code{fun_3}.
#' Defaults to \code{c(1, 2)}.
#' @param scoreNames Optional character vector of length 2 with labels for
#'   the two scores (x- and y-axes). If \code{NULL}, default labels of the form
#'   \code{c("Score i", "Score j")} based on \code{whichScores} are used.
#' @param main Main title for the joint scatterplot.
#' @param xlab,ylab Axis labels for the joint scatterplot. If \code{NULL},
#'   they default to the corresponding \code{scoreNames}.
#' @param pointCexBase Base point size in the joint plot (scaled further by
#'   relative frequency).
#' @param paletteColors Character vector passed to \code{grDevices::colorRampPalette}
#'   to generate the colour scale for point frequencies.
#' @param thresholdCol Colour of threshold lines in all panels.
#' @param thresholdLty Line type of threshold lines.
#' @param thresholdLwd Line width of threshold lines.
#' @param probTextCol Colour of the probability texts in the joint plot.
#' @param probTextCex Character expansion for the probability texts.
#' @param legendN Integer; number of color steps in the embedded legend bar.
#' @param legendXleftRel,legendXrightRel Relative horizontal position of the
#'   colour bar inside the joint panel (0 = left edge, 1 = right edge).
#' @param legendYbottomRel,legendYtopRel Relative vertical position of the
#'   colour bar inside the joint panel (0 = bottom, 1 = top).
#' @param legendLabelOffsetXRel Relative horizontal offset (in plot-width units)
#'   for the min/max labels to the right of the colour bar.
#'
#' @return
#' Called for its side effect of producing a plot. Invisibly returns a list
#' containing the marginals and probabilities for the selected scores.
#' @importFrom graphics layout par abline text rect segments axis
#' @export
plotScoresItemSelection <- function(result,
                                    whichScores  = c(1L, 2L),
                                    scoreNames   = NULL,
                                    main         = "Joint distribution of scores",
                                    xlab = NULL,
                                    ylab = NULL,
                                    pointCexBase = 1.5,
                                    paletteColors = c("red", "yellow", "green"),
                                    thresholdCol  = "blue",
                                    thresholdLty  = 2,
                                    thresholdLwd  = 2,
                                    probTextCol   = "blue",
                                    probTextCex   = 0.8,
                                    legendN       = 40L,
                                    legendXleftRel   = 0.72,
                                    legendXrightRel  = 0.78,
                                    legendYbottomRel = 0.15,
                                    legendYtopRel    = 0.85,
                                    legendLabelOffsetXRel = 0.01) {

  # (1) Basic checks

  if (!is.list(result) || is.null(result$distFun)) {
    stop("`result` must be a list returned by dedaptiveIrt() or fixSelectionIrt(), ",
         "containing element `distFun`.", call. = FALSE)
  }

  distFun <- result$distFun

  if (is.null(result$thres)) {
    stop("`result$thres` is missing. Make sure dedaptiveIrt()/fixSelectionIrt() ",
         "store thresholds as `result$thres`.", call. = FALSE)
  }
  thresAll <- result$thres

  if (!is.numeric(whichScores) || length(whichScores) != 2L) {
    stop("`whichScores` must be a numeric vector of length 2.", call. = FALSE)
  }
  whichScores <- as.integer(whichScores)

  if (max(whichScores) > length(thresAll)) {
    stop("`whichScores` refers to scores for which no threshold is available.",
         call. = FALSE)
  }

  thresUsed <- thresAll[whichScores]

  funCols <- paste0("fun_", whichScores)
  requiredCols <- c(funCols, "freq")
  missingCols <- setdiff(requiredCols, names(distFun))
  if (length(missingCols) > 0L) {
    stop("`distFun` must contain columns: ",
         paste(requiredCols, collapse = ", "),
         ". Missing: ", paste(missingCols, collapse = ", "),
         call. = FALSE)
  }

  if (is.null(scoreNames)) {
    scoreNames <- paste("Score", whichScores)
  } else if (length(scoreNames) != 2L) {
    stop("`scoreNames` must be a character vector of length 2 or NULL.",
         call. = FALSE)
  }

  fun1 <- distFun[[funCols[1]]]
  fun2 <- distFun[[funCols[2]]]
  freq <- distFun$freq

  # 2) Marginals & probabilities

  marg1 <- tapply(freq, fun1, sum)
  marg2 <- tapply(freq, fun2, sum)

  prob1 <- sum(freq[fun1 >= thresUsed[1]])
  prob2 <- sum(freq[fun2 >= thresUsed[2]])

  xRange <- range(fun1, na.rm = TRUE)
  yRange <- range(fun2, na.rm = TRUE)

  freqMin <- min(freq[freq > 0], na.rm = TRUE)
  freqMax <- max(freq, na.rm = TRUE)

  # 3) Layout: 2x2 (top-left: marg1, bottom-left: joint, bottom-right: marg2)

  layoutMatrix <- matrix(c(2, 0,
                           1, 3),
                         nrow = 2, byrow = TRUE)
  layout(layoutMatrix,
         widths  = c(4, 1.5),
         heights = c(1.5, 4))

  paletteFun <- grDevices::colorRampPalette(paletteColors)
  freqScaled <- freq / max(freq, na.rm = TRUE)
  colVec <- paletteFun(100L)[cut(freqScaled, breaks = 100L, include.lowest = TRUE)]

  # 4) Joint scatterplot (panel 1)

  par(mar = c(4.5, 4.5, 2, 2))

  if (is.null(xlab)) xlab <- scoreNames[1]
  if (is.null(ylab)) ylab <- scoreNames[2]

  plot(fun1, fun2,
       xlab = xlab,
       ylab = ylab,
       main = main,
       pch  = 16,
       col  = colVec,
       cex  = pointCexBase * sqrt(freqScaled + 1e-8),
       xlim = xRange,
       ylim = yRange)

  abline(v = thresUsed[1], lty = thresholdLty, lwd = thresholdLwd, col = thresholdCol)
  abline(h = thresUsed[2], lty = thresholdLty, lwd = thresholdLwd, col = thresholdCol)

  xSpan <- xRange[2] - xRange[1]
  ySpan <- yRange[2] - yRange[1]

  xText1 <- thresUsed[1] + 0.05 * xSpan
  yText1 <- yRange[2] - 0.05 * ySpan
  text(xText1, yText1,
       labels = paste0("P(", scoreNames[1], " \u2265 ", thresUsed[1], ") = ",
                       formatC(prob1, digits = 3, format = "f")),
       col = probTextCol, cex = probTextCex, adj = c(0, 0.5))

  xText2 <- xRange[1] + 0.05 * xSpan
  yText2 <- thresUsed[2] + 0.05 * ySpan
  text(xText2, yText2,
       labels = paste0("P(", scoreNames[2], " \u2265 ", thresUsed[2], ") = ",
                       formatC(prob2, digits = 3, format = "f")),
       col = probTextCol, cex = probTextCex, adj = c(0, 0.5))

  ## Embedded colour legend (position controlled by *Rel arguments)
  usr <- par("usr")
  xBar0 <- usr[1] + legendXleftRel  * (usr[2] - usr[1])
  xBar1 <- usr[1] + legendXrightRel * (usr[2] - usr[1])
  yBar0 <- usr[3] + legendYbottomRel * (usr[4] - usr[3])
  yBar1 <- usr[3] + legendYtopRel    * (usr[4] - usr[3])

  yCuts <- seq(yBar0, yBar1, length.out = legendN + 1L)
  colsBar <- paletteFun(legendN)

  par(xpd = NA)
  for (i in seq_len(legendN)) {
    rect(xleft = xBar0,
         ybottom = yCuts[i],
         xright = xBar1,
         ytop = yCuts[i + 1L],
         col = colsBar[i],
         border = NA)
  }
  xLabel <- xBar1 + legendLabelOffsetXRel * (usr[2] - usr[1])
  text(xLabel, yBar0,
       labels = paste0("min=", formatC(freqMin, digits = 3, format = "f")),
       adj = c(0, 0.5), cex = 0.7)
  text(xLabel, yBar1,
       labels = paste0("max=", formatC(freqMax, digits = 3, format = "f")),
       adj = c(0, 0.5), cex = 0.7)
  par(xpd = FALSE)

  ## 4) Top marginal for Score 1 (panel 2) -----------------------------------

  par(mar = c(0, 4.5, 2, 2))

  unique1 <- as.numeric(names(marg1))
  maxMarg1 <- max(marg1, na.rm = TRUE)

  plot(NA,
       xlim = xRange,
       ylim = c(0, maxMarg1),
       xlab = "",
       ylab = "",
       axes = FALSE)

  for (i in seq_along(unique1)) {
    segments(x0 = unique1[i], y0 = 0,
             x1 = unique1[i], y1 = marg1[as.character(unique1[i])],
             lwd = 6, col = "grey80")
  }
  axis(2)
  abline(v = thresUsed[1], col = probTextCol, lwd = 2, lty = 2)

  ## 5) Right marginal for Score 2 (panel 3) ---------------------------------

  par(mar = c(4.5, 0, 2, 2))

  unique2 <- as.numeric(names(marg2))
  maxMarg2 <- max(marg2, na.rm = TRUE)

  plot(NA,
       xlim = c(0, maxMarg2),
       ylim = yRange,
       xlab = "",
       ylab = "",
       axes = FALSE)

  for (i in seq_along(unique2)) {
    segments(x0 = 0, y0 = unique2[i],
             x1 = marg2[as.character(unique2[i])], y1 = unique2[i],
             lwd = 6, col = "grey80")
  }
  axis(1)
  abline(h = thresUsed[2], col = probTextCol, lwd = 2, lty = 2)

  ## 6) Reset layout and return ----------------------------------------------

  layout(1)

  invisible(list(
    whichScores = whichScores,
    thresUsed   = thresUsed,
    marg1       = marg1,
    marg2       = marg2,
    prob1       = prob1,
    prob2       = prob2,
    freqMin     = freqMin,
    freqMax     = freqMax
  ))
}
