#' Export scan output to csv Files
#'
#' @param object An scdf
#' @param filename Character string with the filename.
#' @param sep Sign for data seperator.
#' @param dec Sign for decimal seperator.
#' @param type ...
#' @param caption ...
#' @param flip ...
#' @param kable.style = list with arguments passed to the kable_styling function, 
#' @param cols ...
#' @param ... Further Arguments passed to the kable function.
#' @return  Returns a specif formated html.
#' @export
       

export <- function(object, filename = NULL, sep = ",", dec = ".", type = "html", caption = "", flip = FALSE, kable.style = list(), cols, ...) {
  print(.opt$function_debugging_warning)  

  default.kable.style <- list(bootstrap_options = c("bordered", "condensed"),
                              full_width = FALSE,
                              position = "left")
  tmp <- which(!(names(default.kable.style) %in% names(kable.style)))
  kable.style <- c(kable.style, default.kable.style[tmp])
  
  if (!requireNamespace("kableExtra", quietly = TRUE) ||
      !requireNamespace("knitr", quietly = TRUE) || 
      !requireNamespace("htmlTable", quietly = TRUE))
    stop("Packages knitr, kableExtra, and htmlTable needed for this function to work. Please make sure they are installed.", call. = FALSE)
  
  if(!any(class(object) %in% c("sc","scdf")) ) 
    stop("export can only handle objects returned by scan.\n")
  x <- class(object)[2]
  out <- NULL
  if(x == "autocorr")
    out <- object$autocorr

  ##### trend #####
  if(x == "trend") {
    caption <- c(
      "<i>Trend analysis.</i>"
    )
    
    #footnote <- paste0(footnote, collapse = "; ")
    #caption  <- paste0(caption)
    
    out <- object$trend

    if(isTRUE(flip))
      out <- t(out)
    tmp.rownames <- rownames(out) 
    rownames(out) <- NULL
    for(tmp in object$formulas)
      tmp.rownames <- gsub(paste0(tmp,"."),"",tmp.rownames)
    out <- cbind(Phase = tmp.rownames,out)
    
    arg <- list(out,align = rep("c",ncol(out)), ...)
    table <- do.call(kable,arg)

    arg <- c(list(kable_input = table), kable.style)
    table <- do.call(kable_styling, arg)
    
    for(i in 1:length(object$formulas)) {
      table <- group_rows(table, object$formulas[i], 
      1+(i-1)*(length(object$design)+1), 
      i*(length(object$design)+1))
    }
    tmp <- attributes(table)
    table <- paste0(caption,
                    table)
    attributes(table) <- tmp
    
  }

  ##### describe #####
  if(x == "describe") {
    caption  <- "<i>Descriptive statistics.</i>"
    footnote <- c("Note: n = Number of measurements",
                  "Missing = Number of missing values",
                  "M = Mean",
                  "Median = Median",
                  "SD = Standard deviation",
                  "MAD = Median average deviation",
                  "Min = Minimum",
                  "Max = Maximum",
                  "Trend = Slope of dependent variable regressed on measurement-time."
    )
    footnote <- paste0(footnote, collapse = "; ")

    n.phases <- length(object$design)
    out <- object$descriptives
    colnames(out) <- rep(object$design, 9)

    table <- kable(out, align = c("c", rep("c",9*n.phases)),...)
    arg <- c(list(kable_input = table), kable.style)
    table <- do.call(kable_styling, arg)
    table <- add_header_above(table, 
            c(" " = 1, "n" = n.phases, 
              "Missing" = n.phases, 
              "M" = n.phases,
              "Median" = n.phases,
              "SD" = n.phases,
              "MAD" = n.phases,
              "Min" = n.phases,
              "Max" = n.phases,
              "Trend" = n.phases
            )
    )
    tmp <- attributes(table)
    table <- paste0(caption,
                    table,
                    footnote)
    attributes(table) <- tmp
    #table <- footnote(table, general = footnote)
    
  }

  ##### overlap #####
  if(x == "overlap") {
    caption <- c(
      "<i>Overlap indices.",
      .stringPhasesSC(object$design[object$phases.A],object$design[object$phases.B]),
      ".</i>"
    )
    footnote <- c("PND = Percentage Non-Overlapping Data",
                  "PEM = Percentage Exceeding the Median",
                  "PET = Percentage Exceeding the Trend",
                  "NAP = Nonoverlap of all pairs",
                  "NAP-R = NAP rescaled",
                  "PAND = Percentage all nonoverlapping data",
                  "Tau U = Parker's Tau-U",
                  "Delta M = Mean difference between phases",
                  "Delta Trend = Trend difference between phases",
                  "SMD = Standardized Mean Difference."
                  )
    footnote <- paste0(footnote, collapse = "; ")
    caption  <- paste0(caption, collapse = "")
    n.phases <- 2
    out <- object$overlap
    
    colnames(out)[5] <- "NAP-R"
    colnames(out)[7] <- "Tau-U"
    colnames(out)[8] <- "Delta M"
    colnames(out)[9] <- "Delta Trend"
    
    if(isTRUE(flip))
       out <- t(out)
    table <- kable(out, align = rep("c",ncol(out)), ...)
    arg <- c(list(kable_input = table), kable.style)
    table <- do.call(kable_styling, arg)
    tmp <- attributes(table)
    table <- paste0(caption,
                    table,
                    footnote)
    attributes(table) <- tmp
  }
  
  ##### pr #####
  if(x == "pr"){
    if(object$ar == 0)
      out <- summary(object$full.model)$coefficients
    if(object$ar > 0)
      out <- summary(object$full.model)$tTable
    if(nrow(out) == 1) {
      out <- cbind(out[,1, drop = FALSE], t(suppressMessages(confint(object$full))), out[, 2:4, drop = FALSE])
    } else out <- cbind(out[, 1], suppressMessages(confint(object$full)), out[, 2:4])
    
    out <- as.data.frame(out)
    if(!is.null(object$r.squares))
      out$R2 <- c("", format(round(object$r.squares, 2)))
    
    rn <- rownames(out)
    if(!is.na(match("mt",rn)))
      rownames(out)[match("mt",rn)] <- "Trend"
    if(!is.na(match(attr(object, .opt$mt),rn)))
      rownames(out)[match(attr(object, .opt$mt),rn)] <- paste0("Trend ", attr(x,.opt$mt))
    
    if(!is.na(match("(Intercept)",rn)))
      rownames(out)[match("(Intercept)",rn)] <- "Intercept"
    
    PHASE <- attr(object, .opt$phase)
    
    rownames(out) <- gsub(PHASE,paste0("Level ", PHASE," "),rownames(out))
    rownames(out) <- gsub("inter",paste0("Slope ", PHASE," "),rownames(out))
    
    if(!is.null(object$r.squares))
      colnames(out) <- c("B","2.5%","97.5%","SE", "t","p", "Delta R\u00b2")		
    if(is.null(object$r.squares))
      colnames(out) <- c("B","2.5%","97.5%","SE", "t","p")		
    
    if(object$family == "poisson" || object$family == "nbinomial") {
      OR <- exp(out[,1:3])
      Q  <- (OR-1)/(OR+1)
      out <- cbind(out[, -7], round(OR, 3), round(Q, 2))
      colnames(out) <- c("B","2.5%","97.5%","SE", "t","p", "Odds Ratio","2.5%", "97.5%","Yule's Q","2.5%", "97.5%")		
      
      Chi <- object$full$null.deviance - object$full$deviance
      DF  <- object$full$df.null - object$full$df.residual
      test <- sprintf("\u0347\u00b2(%d) = %.2f; <i>p</i> = %0.3f; <i>AIC</i> = %.0f", DF, Chi, 1 - pchisq(Chi, df = DF), object$full$aic)
    }
    
    out <- cbind(Parameter = rownames(out),out)
    
    if(object$family == "gaussian") {
      out[,2] <- sprintf("%.2f",out[,2])
      out[,3] <- sprintf("%.2f",out[,3])
      out[,4] <- sprintf("%.2f",out[,4])
      out[,5] <- sprintf("%.2f",out[,5])
      out[,6] <- sprintf("%.2f",out[,6])
      out[,7] <- .nice.p((out[,7]))
      out[,8] <- gsub("^0\\.",".",out[,8])
      #test <- sprintf("<i>F</i>(%d, %d) = %.2f; <i>p</i> = %0.3f; <i>R</i>\u00b2 = %0.3f; Adjusted <i>R</i>\u00b2 = %0.3f", object$F.test["df1"], object$F.test["df2"],object$F.test["F"], object$F.test["p"], object$F.test["R2"], object$F.test["R2.adj"])
      test <- sprintf("F(%d, %d) = %.2f; p = %0.3f; R\u00b2 = %0.3f; Adjusted R\u00b2 = %0.3f", object$F.test["df1"], object$F.test["df2"],object$F.test["F"], object$F.test["p"], object$F.test["R2"], object$F.test["R2.adj"])
      
    }
    
    out$Parameter <- as.character(out$Parameter)
  
    if(identical(caption, ""))
      caption <- paste0("Piecewise-regression model for variable '", attr(object,.opt$dv),"'.")

    if(identical(type, "html")) {
      table <- kable(out, row.names = FALSE, align = "lcccccc",...)
      arg <- c(list(kable_input = table), kable.style)
      table <- do.call(kable_styling, arg)
      
      table <- add_header_above(table, c(" " = 2, "CI(95%)" = 2, " " = 4))
      tmp <- attributes(table)
      table <- footnote(table, general = test)
      table <- paste0(#"<p>Table.<br>\n",
                      "<i>",caption, "</i>",
                      table)
                      #test)
      attributes(table) <- tmp
    }
    
    if(identical(type, "html2")) {
      table <- htmlTable(out,
        cgroup = c("","", "CI (95%)","","","",""),
        n.cgroup = c(1,1,2,1,1,1),
        align = "lc",
        align.header = "lc",
        caption = caption,
        tfoot = paste0("<i>Note:</i> ",test),
        rnames = FALSE, ...)
    }

  } 
  
  ##### hplm #####
  if(x == "hplm") {
    if(identical(caption, ""))
      caption <- paste0(#"<p>Table.<br>\n<i>",
                        "Hierarchical Piecewise Linear Regression for variable '", attr(object,.opt$dv),"'.</i><br>")
    
    Summary <- summary(object$hplm)
    if(object$model$ICC)
      ICC <- sprintf("<i>ICC</i> = %.3f, <i>L</i> = %.1f, <i>p</i> = %.3f", object$ICC$value, object$ICC$L, object$ICC$p)
    else 
      ICC <- ""
    
    footnote <- c(paste0("Estimation method ",object$model$estimation.method),
                  paste0("Slope estimation method: ",object$model$interaction.method),
                  paste0(object$N," cases")#,
                  #paste0("AIC = ", round(Summary$AIC,1)),
                  #paste0("BIC = ", round(Summary$BIC,1)),
                  #ICC
                )
    # footnote <- c(paste0("<i>Note:</i> Estimation method ",object$model$estimation.method),
    #               paste0("Slope estimation method: ",object$model$interaction.method),
    #               paste0(object$N," cases"),
    #               paste0("<i>AIC</i> = ", round(Summary$AIC,1)),
    #               paste0("<i>BIC</i> = ", round(Summary$BIC,1)),
    #               ICC
    # )
    footnote <- paste0(footnote, collapse = "; ")
   
    out <- as.data.frame(summary(object$hplm)$tTable)
    rn <- rownames(out)
    if(!is.na(match("mt",rn)))
      rownames(out)[match("mt",rn)] <- "Trend"
    if(!is.na(match(attr(object, .opt$mt),rn)))
      rownames(out)[match(attr(object, .opt$mt),rn)] <- paste0("Trend ", attr(object,.opt$mt))
    
    if(!is.na(match("(Intercept)",rn)))
      rownames(out)[match("(Intercept)",rn)] <- "Intercept"
    
    PHASE <- attr(object, .opt$phase)
    
    rownames(out) <- gsub(PHASE,paste0("Level ", PHASE," "),rownames(out))
    rownames(out) <- gsub("inter",paste0("Slope ", PHASE," "),rownames(out))
    
    colnames(out) <- c("B","SE","df","t","p")
    
    SD <- round(as.numeric(VarCorr(object$hplm)[,"StdDev"]),3)
    md <- data.frame("SD" = SD)
    rownames(md) <- names(VarCorr(object$hplm)[,2])
    rn <- rownames(md)
    if(!is.na(match("mt",rn)))
      rownames(md)[match("mt",rn)] <- "Trend"
    if(!is.na(match(attr(object, .opt$mt),rn)))
      rownames(md)[match(attr(object, .opt$mt),rn)] <- paste0("Trend ", attr(object, .opt$mt))
    if(!is.na(match("(Intercept)",rn)))
      rownames(md)[match("(Intercept)",rn)] <- "Intercept"
    
    rownames(md) <- gsub(PHASE,paste0("Level ", PHASE," "),rownames(md))
    rownames(md) <- gsub("inter",paste0("Slope ", PHASE," "),rownames(md))
    
    if(object$model$lr.test) {
      if(is.null(object$LR.test[[1]]$L.Ratio)) {
        object$LR.test[[1]]$L.Ratio <- NA
        object$LR.test[[1]]$"p-value" <- NA
        object$LR.test[[1]]$df <- NA
      }
      
      md$L  <- c(round(unlist(lapply(object$LR.test, function(x) x$L.Ratio[2])), 2),NA)
      md$df <- c(unlist(lapply(object$LR.test, function(x) {x$df[2]-x$df[1]})), NA)
      md$p  <- c(round(unlist(lapply(object$LR.test, function(x) x$"p-value"[2])), 3), NA)
    }
  
    out$p <- .nice.p(out$p)
    md$p <- .nice.p(md$p)
   
    out[,] <- lapply(out[,], function(x) 
      if(class(x) == "numeric") as.character(round(x,2)) else x)
    out <- cbind(Parameter = rownames(out), out, stringsAsFactors=FALSE)
    rownames(out) <- NULL
    md[,] <- lapply(md[,], function(x) 
      if(class(x) == "numeric") as.character(round(x,2)) else x)
    md <- cbind(" " = rownames(md), md, stringsAsFactors=FALSE)
    rownames(md) <- NULL

        tmp.nrow <- nrow(out)
    out[(tmp.nrow+1):(tmp.nrow+nrow(md)+1+3),] <- ""
    
    out[(tmp.nrow+1):(tmp.nrow+nrow(md)+1),1:ncol(md)] <- 
      rbind(colnames(md), md, stringsAsFactors=FALSE)
    
    out[tmp.nrow+nrow(md)+2,1:2] <- c("AIC", as.character(round(Summary$AIC,1)))
    out[tmp.nrow+nrow(md)+3,1:2] <- c("BIC", as.character(round(Summary$BIC,1)))
    out[tmp.nrow+nrow(md)+4,1:4] <- 
      c("ICC", as.character(round(object$ICC$value,2)),
        paste0("L=",round(object$ICC$L,1)),
        paste0("p=",round(object$ICC$p,2)))
 
    
    #return(out)
    table <- kable(out, align = c("l",rep("c",ncol(out)-1)),...)
    arg <- c(list(kable_input = table), kable.style)
    table <- do.call(kable_styling, arg)
    
    #table <- group_rows(table, paste0("Fixed effects (",deparse(object$model$fixed),")"),
    #                    1,tmp.nrow)
    table <- group_rows(table, "Random effects",#paste0("Random effects (",deparse(object$model$random),")"),
                        tmp.nrow+1, nrow(out))
    table <- row_spec(table, tmp.nrow+1,bold = TRUE, color = "black")
    table <- row_spec(table, tmp.nrow+nrow(md)+1,hline_after = TRUE)
    table <- row_spec(table, tmp.nrow,hline_after = TRUE)
    #table2 <- kable(md, align = rep("c",ncol(out)), ...)
    #arg <- c(list(kable_input = table2), kable.style)
    #table2 <- do.call(kable_styling, arg)
    
    tmp <- attributes(table)
    #table <- paste0("<br><b>Fixed effects (",deparse(object$model$fixed),")</b>",                    
    #                as.character(table),
    #                "<b>Random effects (",deparse(object$model$random),")</b>")                  
    #                #as.character(table2))
    table <- footnote(table, general = footnote)
    table <- paste0(caption,
                    table)#,
                    #footnote)
    attributes(table) <- tmp
  }
  
  ##### scdf #####
  if(identical(class(object)[1],"scdf")) {
    
    N <- cases <- length(object)

    if(missing(cols))
      cols <- names(object[[1]])
    if(identical(cols,"main"))
      cols = c(attr(object, .opt$phase), attr(object, .opt$dv), attr(object, .opt$mt))
    if(is.null(names(object)))
      names(object) <- paste0("Case",1:N)
 
    nonames <- which(is.na(names(object)))
    names(object)[nonames] <- paste0("Case",nonames)
    max.row <- max(unlist(lapply(object, nrow)))
    for(i in 1:cases){
      n.row <- nrow(object[[i]])
      object[[i]][,attr(object, .opt$phase)] <- as.character(object[[i]][,attr(object, .opt$phase)])
      if(n.row < max.row) 
        object[[i]][(n.row + 1):max.row, names(object[[i]])] <- ""
    }
    rows <- max.row
    out <- lapply(object[1:cases], function(x) x[1:rows,cols])
    names <- lapply(out, names)
    out <- as.data.frame(out)
    names(out) <- unlist(names[1:cases])
    
    table <- kable(out, align = rep("c",ncol(out)), ...)
    arg <- c(list(kable_input = table), kable.style)
    table <- do.call(kable_styling, arg)
    case.names <- rep(ncol(out)/N, N)
    names(case.names) <- names(object)
    table <- add_header_above(table, case.names)
    footnote <- ""
    if(!is.null(attr(object,"info")))
      footnote <- attr(object,"info")
    if(!is.null(attr(object,"author")))
      footnote <- paste0(footnote,"\nAuthor: ",attr(object,"author"))
    if(!identical(footnote,""))
      table <- footnote(table, general = footnote)
  }

  ##### finish #####
  if(!is.null(filename))
    cat(table,filename)
  
  table
}

.nice.p <- function(p) {
  p2 <- ifelse(p >= 0.05, paste0(substring(trunc(p*100)/100,2)),p)
  p2[p<0.05] <- "<.05"	
  p2[p<0.01] <- "<.01"	
  p2[p<0.001] <- "<.001"
  p2
} 
