`diagnostics` <-
function (grp, grpnames, ProjectDir, b, dataObs, dvname,  covplt) 
{
  grph.cwres <- "CWRES" %in% names(dataObs)
  if (is.null(grp)) {
    dataObs$plotrGroup <- 1
    grp <- "plotrGroup"
    grpnames <- "all"
  }
  grps <- sort(unique(dataObs[[grp]]))
  if (is.null(grpnames)) {
    grpnames <- grps
  }
  if (length(grps) != length(grpnames)) {
    grpnames <- grps
    warning(paste("Number of grouping variables does not equal number of grouping names for Run ", 
                  b, ".", sep = ""))
  }
  tad <- is.element("TAD", names(dataObs))
  if (!tad) 
    message(paste("No TAD item for run ", b, ".", sep = ""))
  startDevice(ProjectDir, "DiagnosticPlotReview", b)
  for (j in 1:length(grps)) {
    gnm <- grpnames[j]
    data.t <- dataObs[dataObs[[grp]] == grps[j], ]
    par(mfrow=c(2,2))
    count <- 0
    increment <- function() {
      tiles <- max(cumprod(par("mfrow")))
      if (count%%tiles == 0) 
        mtext(paste("Model ", b, " Group = ", gnm, " [", 
                    grps[j], "]"), line = 0.5, outer = TRUE)
      count <<- count + 1
    }
    xlim <- c(min(0, data.t$DV, data.t$PRED), max(data.t$DV, 
                                                  data.t$PRED))
    plot(y = data.t$DV, x = data.t$PRED, ylab = paste("Observed ", 
                                           dvname), xlab = paste("Predicted ", dvname), ylim = xlim, 
         xlim = xlim)
    abline(c(0, 1))
    increment()
    if (!is.null(data.t$IPRE)) {
      xlim <- c(min(0, data.t$DV, data.t$PRED), max(data.t$DV, 
                                                    data.t$PRED, data.t$IPRE))
      plot(y = data.t$DV, x = data.t$IPRE, ylab = paste("Observed ", 
                                             dvname), xlab = paste("Individual Predicted "), 
           ylim = xlim, xlim = xlim)
      abline(c(0, 1))
      increment()
    }
    plot(y = data.t$RES, x = data.t$PRED, ylab = paste("Residuals"), 
         xlab = paste("Predicted ", dvname))
    abline(h = 0)
    lines(lowess(x = data.t$PRED, y = data.t$RES), lty = 2)
    increment()
    try(plot(y = data.t$RES, x = data.t$TIME, ylab = "Residuals", 
         xlab = "Time (hr)"), silent=TRUE)
    try(abline(h = 0), silent=TRUE)
    try(lines(lowess(x = data.t$TIME, y = data.t$RES), lty = 2), silent=TRUE)
    increment()
    plot(y = data.t$WRES, x = data.t$PRED, ylab = "Weighted Residuals", 
         xlab = paste("Predicted ", dvname))
    abline(h = 0)
    lines(lowess(x = data.t$PRED, y = data.t$WRES), lty = 2)
    increment()
    if (grph.cwres) {
      plot(y = data.t$CWRES, x = data.t$PRED, ylab = paste("Conditional Weighted Residuals"), 
           xlab = paste("Predicted ", dvname))
      abline(h = 0)
      lines(lowess(x = data.t$PRED, y = data.t$CWRES), 
            lty = 2)
      increment()
    }
    try(plot(y = data.t$WRES, x = data.t$TIME, ylab = "Weighted Residuals", 
         xlab = "Time (hr)"), silent=TRUE)
    try(abline(h = 0), silent=TRUE)
    try(lines(lowess(x = data.t$TIME, y = data.t$WRES), lty = 2), silent=TRUE)
    increment()
    if (grph.cwres) {
     try(plot(y = data.t$CWRES, x = data.t$TIME, ylab = paste("Conditional Weighted Residuals"), 
           xlab = paste("Time (hr)")), silent=TRUE)
      try(abline(h = 0), silent=TRUE)
      try(lines(lowess(x = data.t$TIME, y = data.t$CWRES), 
            lty = 2), silent=TRUE)
      increment()
    }
    qqnorm(data.t$WRES, main = "Normal Q-Q Plot of WRES")
    abline(0, 1)
    increment()
    if (grph.cwres) {
      qqnorm(data.t$CWRES, main = "Normal Q-Q Plot of CWRES")
      abline(0, 1)
      increment()
    }
    if(grph.cwres){
      qqplot(data.t$CWRES, data.t$WRES, xlab = "CWRES", ylab = "WRES", 
           main = "Q-Q Plot of WRES vs. CWRES")
      abline(0, 1)
      increment()
    }
    if (grph.cwres) 
      boxplot(data.t$WRES, data.t$CWRES, names = c("WRES", 
                                           "CWRES"), main = "Boxplots of (C)WRES")
    if (!grph.cwres) 
      boxplot(data.t$WRES, names = "WRES", main = "Boxplot of WRES")
    increment()
    if (tad) {
      plot(y = data.t$RES, x = data.t$TAD, ylab = "Residuals", 
           xlab = "Time After Dose (hr)")
      abline(h = 0)
      lines(lowess(x = data.t$TAD, y = data.t$RES), lty = 2)
      increment()
      plot(y = data.t$WRES, x = data.t$TAD, ylab = "Weighted Residuals", 
           xlab = "Time After Dose (hr)")
      abline(h = 0)
      lines(lowess(x = data.t$TAD, y = data.t$WRES), lty = 2)
      increment()
      if (grph.cwres) {
        plot(y = data.t$CWRES, x = data.t$TAD, ylab = "Conditional Weighted Residuals", 
             xlab = "Time After Dose (hr)")
        abline(h = 0)
        lines(lowess(x = data.t$TAD, y = data.t$CWRES), 
              lty = 2)
        increment()
      }
    }
  }
  if(covplt==0) stopDevice()
}

