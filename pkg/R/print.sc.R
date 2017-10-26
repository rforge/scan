
print.sc <- function(x, ...) {
  value <- class(x)[2]

    
  if(value == "autocorr") {
    cat("Autocorrelations\n\n")
    x <- x$autocorr
    print(x)
  }
  
  
  if(value == "overlap") {
    cat("Overlap Indices\n\n")
    cat("Design: ", x$design, "\n")
    cat("Comparing phase", unlist(x$phases.A),"to",unlist(x$phases.B),"\n\n")

    print(round(t(x$overlap),2),...)
  }

  
  if(value == "TAU-U") {	
    cat("Overall Tau-U: \n")
    print(x$Overall_tau_u)
    cat("\n\n")
    out <- lapply(x$table,function(x)round(x,3))
    arg <- list(...)
    complete <- FALSE
    if(any(names(arg) == "complete")) {
      complete <- arg$complete
    }
    if(complete == FALSE)
      out <- lapply(x$table,function(x)round(x[c(-1,-2,-3,-4),c(-1,-2,-3,-4, -9, -10)],3))
    
    print(out)
    #print(x$table)
  }
  
  if(value == "power") {	
    cat("Test-Power in percent:\n")
    ma <- matrix(unlist(x[1:16])*100,byrow = FALSE, ncol = 2, dimnames = list(c("tauU: A vs. B - Trend A",paste0("Rand-Test: ",x$rand.test.stat[1]),  "PLM.Norm: Level", "PLM.Norm: Slope", "PLM.Poisson: Level", "PLM.Poisson: Slope", "HPLM: Level", "HPLM: Slope"), c("Power", "Alpha-error")))
    print(ma)
  }
  
  if(value == "PET") {	
    cat("Percent Exceeding the Trend\n\n")
    cat("N cases = ", x$N,"\n")
    cat("\n")
    ma <- cbind(x$PET, x$p, x$PET.ci)
    colnames(ma) <- c("PET","binom.p", "PET CI")
    rownames(ma) <- x$case.names
    print(round(ma,3))
    cat("\n")
    
    if(x$decreasing) {
      cat("Assumed decreasing values in the B-phase.\n\n")
      cat("Binom.test: alternative hypothesis: true probability < 50%\n")
      cat(sprintf("PET CI: Percent of values less than lower %d%% confidence threshold (smaller %.3f*se below predicted value)\n",x$ci,x$se.factor))
    } else {
      cat("Binom.test: alternative hypothesis: true probability > 50%\n")
      cat(sprintf("PET CI: Percent of values greater than upper %d%% confidence threshold (greater %.3f*se above predicted value)\n",x$ci,x$se.factor))
    }
    
  }	
  
  if(value == "NAP") {
    cat("Nonoverlap of All Pairs\n\n")
    cat("NAP = ", x$NAP, "%\n")
    cat("NAP rescaled = ", x$NAP.rescaled, "%\n")
  }
  
  if(value == "PEM") {
    cat("Percent Exceeding the Median\n\n")
    ma <- cbind(PEM = x$PEM, x$test)
    print(round(ma,3))
    cat("\n")
    if(x$decreasing) {
      cat("Assumed decreasing values in the B-phase.\n\n")
      cat("Alternative hypothesis: true probability < 50%\n")
    } else {
      cat("Alternative hypothesis: true probability > 50%\n")
    }
  }
  
  if(value == "PND") {
    cat("Percent Non-Overlapping Data\n\n")
    cat(paste("Case ", 1:length(x$PND), ": ",round(x$PND,2), "%",sep = ""), sep = "\n")
    cat("Mean  :", round(mean(x$PND, na.rm = TRUE),2),"%\n")
  }	
  
  if(value == "trend") {
    x$trend <- round(x$trend,3)
    cat("Trend in phases A and B\n\n")
    #cat("N cases = ", x$N,"\n")
    #cat("\n")
    print(x$trend)
    cat("\n")
    cat("Note. Measurement-times of phase B start at", 1+x$B.offset, "\n")
  }
  
  
  if(value == "rci") {
    cat("!!! Caution! This function is under development and not yet ready for use!!!\n\n")
    cat("Reliable Change Index\n\n")
    cat("Mean Difference = ", x$descriptives[2,2] - x$descriptives[1,2], "\n")
    
    cat("Standardized Difference = ", x$stand.dif, "\n")
    cat("\n")
    cat("Descriptives:\n")
    print(x$descriptives)
    cat("\n")
    cat("Reliability = ", x$reliability, "\n")
    cat("\n")
    cat(x$conf.percent*100,"% Confidence Intervals:\n")
    print(x$conf)
    cat("\n")
    cat("Reliable Change Indices:\n")
    print(x$RCI)
    cat("\n")
  }
  
  if(value == "rand") {
    cat("Randomization Test\n\n")
    if (x$N > 1)
      cat("Multiple-Baseline Test for", x$N, "cases.\n\n")
    cat("Statistic: ",x$statistic,"\n")
    if(is.na(x$startpoints[1])) {
      cat("Minimal length of each phase: ", x$limit, "\n")
    } else {
      cat("Possible starting points of phase B: ", x$startpoints, "\n")
    }
    cat("Observed statistic = ", x$observed.statistic, "\n")
    cat("\n")
    if(x$auto.corrected.number)
      cat("Warning! The assigned number of random permutations exceeds the number of possible permutations.\nAnalysis is restricted to all possible permutations.\n")
    if(x$complete) {
      cat("\nDistribution based on all", x$possible.combinations,"possible combinations.\n")
    } else 
      cat("\nDistribution based on a random sample of all", x$possible.combinations, "possible combinations.\n")
    
    #cat("\nDistribution:\n")
    cat("n   = ", x$number,"\n")
    cat("M   = ", mean(x$distribution),"\n")
    cat("SD  = ", sd(x$distribution),"\n")
    cat("Min = ", min(x$distribution),"\n")
    cat("Max = ", max(x$distribution),"\n")
    cat("\n")
    if(x$p.value == 0)
      cat("p   < ", format(1/x$number, scientific = FALSE), "\n")
    else
      cat("p   = ", x$p.value, "\n")
    if(x$number > 3 & x$number < 5001) {
      sh <- shapiro.test(x$distribution)
      cat(sprintf("\nShapiro-Wilk Normality Test: W = %0.3f; p = %0.3f",sh[[1]], sh$p.value))
      if (sh$p.value > .05)
        cat("  (Hypothesis of Normality maintained)\n")
      else
        cat("  (Hypothesis of Normality rejected)\n")
    } else cat("\nSample size must be between 3 and 5000 to perform a Shapiro-Wilk Test.\n")
    cat(sprintf("z = %0.4f, p = %0.4f (single sided)\n", x$Z, x$p.Z.single))
  }
  
  if(value == "hplm") {
    
    cat("Hierarchical Piecewise Linear Regression\n\n")
    
    cat("Estimation method",x$model$estimation.method,"\n")
    cat("Slope estimation method:",x$model$interaction.method,"\n")
    cat(x$N,"Cases\n\n")
    
    out <- list()
    
    if(x$model$ICC) {
      out$ICC <- sprintf("ICC = %.3f; L = %.1f; p = %.3f\n\n", x$ICC$value, x$ICC$L, x$ICC$p)
      cat(out$ICC)
    }
    
    md <- as.data.frame(summary(x$hplm)$tTable)

    colnames(md) <- c("B","SE","df","t","p")
    rn <- rownames(md)
    if(!is.na(match("mt",rn)))
      rownames(md)[match("mt",rn)] <- "Trend"
    if(!is.na(match("(Intercept)",rn)))
      rownames(md)[match("(Intercept)",rn)] <- "Intercept"
    
    rownames(md) <- gsub("phase","Level Phase ",rownames(md))
    rownames(md) <- gsub("inter","Slope Phase ",rownames(md))
    
    md$B  <- round(md$B, 3)
    md$SE <- round(md$SE,3)
    md$t  <- round(md$t, 3)
    md$p  <- round(md$p, 3)
    
    out$ttable <- md
    
    cat("Fixed effects (",deparse(x$model$fixed),")\n\n")
    print(md)
    
    cat("\nRandom effects (",deparse(x$model$random),")\n\n")
    SD <- round(as.numeric(VarCorr(x$hplm)[,"StdDev"]),3)
    md <- data.frame("EstimateSD" = SD)
    rownames(md) <- names(VarCorr(x$hplm)[,2])
    rn <- rownames(md)
    if(!is.na(match("mt",rn)))
      rownames(md)[match("mt",rn)] <- "Trend"
    if(!is.na(match("(Intercept)",rn)))
      rownames(md)[match("(Intercept)",rn)] <- "Intercept"
    
    rownames(md) <- gsub("phase","Level Phase ",rownames(md))
    rownames(md) <- gsub("inter","Slope Phase ",rownames(md))
      
    if(x$model$lr.test) {
      if(is.null(x$LR.test[[1]]$L.Ratio)) {
        x$LR.test[[1]]$L.Ratio <- NA
        x$LR.test[[1]]$"p-value" <- NA
        x$LR.test[[1]]$df <- NA
      }
      
      md$L  <- c(round(unlist(lapply(x$LR.test, function(x) x$L.Ratio[2])), 2),NA)
      md$df <- c(unlist(lapply(x$LR.test, function(x) {x$df[2]-x$df[1]})), NA)
      md$p  <- c(round(unlist(lapply(x$LR.test, function(x) x$"p-value"[2])), 3), NA)
    }
    
    print(md, na.print = "-")
    invisible(out)
  }
  
  if(value == "pr") {
    cat("Piecewise Regression Analysis\n\n")
    cat("Regression model: ", x$model,"\n\n")
    cat("Fitted a", x$family, "distribution.\n\n")		
    
    if (x$ar > 0)
      cat("Correlated residuals up to autoregressions of lag", x$ar, "are modelled\n\n")
    
    if(x$family == "poisson" || x$family == "nbinomial") {
      Chi <- x$full$null.deviance - x$full$deviance
      DF <- x$full$df.null - x$full$df.residual
      cat(sprintf("X-Square(%d) = %.2f; p = %0.3f; AIC = %.0f\n\n", DF, Chi, 1 - pchisq(Chi, df = DF), x$full$aic))	
    } else {
      cat(sprintf("F(%d, %d) = %.2f; p = %0.3f; R-Square = %0.3f; Adjusted R-Square = %0.3f\n\n", x$F.test["df1"], x$F.test["df2"], x$F.test["F"], x$F.test["p"], x$F.test["R2"], x$F.test["R2.adj"]))	
    }
    
    
    if(x$ar == 0)
      res <- summary(x$full.model)$coefficients
    if(x$ar > 0)
      res <- summary(x$full.model)$tTable

    res <- cbind(res[,1], suppressMessages(confint(x$full)), res[,2:4])
    res <- round(res,3)
    res <- as.data.frame(res)
    res$R2 <- c("",format(round(x$r.squares,4)))
    rn <- rownames(res)
    if(!is.na(match("mt",rn)))
      rownames(res)[match("mt",rn)] <- "Trend"
    if(!is.na(match("(Intercept)",rn)))
      rownames(res)[match("(Intercept)",rn)] <- "Intercept"
    
    #if(!is.na(match("phaseB",rn)))
    #  rownames(res)[match("phaseB",rn)] <- "Level"
    rownames(res) <- gsub("phase","Level Phase ",rownames(res))
    rownames(res) <- gsub("inter","Slope Phase ",rownames(res))
    
        #if(!is.na(match("inter",rn)))
    #  rownames(res)[match("inter",rn)] <- "Slope"
     
    colnames(res) <- c("B","2.5%","97.5%","SE", "t","p", "R-Square")		
    if(x$family == "poisson" || x$family == "nbinomial") {
      OR <- exp(res[,1:3])
      Q <- (OR-1)/(OR+1)
      res <- cbind(res[,-7], round(OR,3), round(Q,2))
      colnames(res) <- c("B","2.5%","97.5%","SE", "t","p", "Odds Ratio","2.5%", "97.5%","Yule's Q","2.5%", "97.5%")		
    }
    print(res)
    cat("\n")
    cat("Autocorrelation of the Residuals\n")
    print(data.frame(lag = 1:5,r = round(acf(residuals(x$full.model), lag.max = 5,plot = FALSE)$acf[2:6],2)))
    cat("\n")
  }
  
  if(value == "PAND") {
    cat("Percentage of all non-overlapping data\n\n")
    cat("PAND = ", round(x$PAND,1), "%\n")
    cat("Phi = ", round(x$phi,3), " ; Phi-Square = ", round(x$phi^2,3), "\n\n")
    cat("Number of Cases: ", x$N, "\n")
    cat("Total measurements: ", x$n, "\n")
    cat("in phase A: ", x$nA, "\n")
    cat("in phase B: ", x$nB, "\n")
    cat("n overlapping data per case: ")
    cat(x$OD.PP, sep = ", ")
    cat("\n")
    cat("n Overlapping data: ",x$OD, "\n")
    cat("% Overlapping data: ",round(x$POD,1), "\n")
    ma <- x$matrix
    cat("\n")
    cat("2 x 2 Matrix of proportions\n")
    cat("\t% expected\n")
    
    cat("\tA\tB\ttotal\n")
    cat("%    A",round(ma[1,]*100,1), sum(round(ma[1,]*100,1)), sep = "\t")
    cat("\n")
    cat("real B",round(ma[2,]*100,1), sum(round(ma[2,]*100,1)), sep = "\t")
    cat("\n")
    cat(" total",sum(round(ma[,1]*100,1)), sum(round(ma[,2]*100,1)), sep = "\t")
    cat("\n")
    ma <- x$matrix.counts
    cat("\n")
    cat("2 x 2 Matrix of counts\n")
    cat("\texpected\n")
    
    cat("\tA\tB\ttotal\n")
    cat("     A",round(ma[1,],1), sum(round(ma[1,],1)), sep = "\t")
    cat("\n")
    cat("real B",round(ma[2,],1), sum(round(ma[2,],1)), sep = "\t")
    cat("\n")
    cat(" total",sum(round(ma[,1],1)), sum(round(ma[,2],1)), sep = "\t")
    cat("\n")
    cat("\n")
    if(x$correction)
      cat("\nNote. Matrix is corrected for ties\n")
    cat("\nCorrelation based analysis:\n\n")
    out <- sprintf("z = %.3f, p = %.3f, Tau = %.3f",x$correlation$statistic, x$correlation$p.value, x$correlation$estimate)
    cat(out,"\n")
  }
  
  if(value == "describe") {
    cat("Describe Single-Case Data\n\n")
    cat("Design: ", x$design, "\n\n")
    print(round(t(x$descriptives),2), ...)
    
    return()
 
  }	
  
  if(value == "outlier") {
    cat("Outlier Analysis for Single-Case Data\n\n")
    
    if (x$criteria[1] == "CI") {
      names(x$ci.matrix) <- x$case.names
      cat("Criteria: Exceeds", as.numeric(x$criteria[2])*100,"% Confidence Interval\n\n")
      print(x$ci.matrix)
    }
    if (x$criteria[1] == "SD") {
      names(x$sd.matrix) <- x$case.names
      cat("Criteria: Exceeds", x$criteria[2], "Standard Deviations\n\n")
      print(x$sd.matrix)
    }
    if (x$criteria[1] == "MAD") {
      names(x$mad.matrix) <- x$case.names
      cat("Criteria: Exceeds", x$criteria[2], "Mean Average Deviations\n\n")
      print(x$mad.matrix)
    }
    if (x$criteria[1] == "Cook") {
      cat("Criteria: Cook's Distance based on piecewise-linear-regression exceeds", x$criteria[2],"\n\n")
    }
    for(i in 1:length(x$dropped.n)) {
      cat("Case",x$case.names[i],": Dropped",x$dropped.n[[i]],"\n")
    }
    cat("\n")
  }
}
