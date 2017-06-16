

.SCfill <- function(x, y, ymin, col = "grey") {
  for(i in 1:length(x))
    polygon(c(x[i], x[i+1], x[i+1], x[i]),c(ymin,ymin, y[i+1],y[i]), col=col, border = NA)
}

newplotSC <- function(data, ylim = NULL, xlim = NULL, fill = "", lines = "", marks = NULL, annotations = NULL, phase.names = NULL, FUN.AB = NULL, xlab = "Measurement time", ylab = "Score", text.ABlag = NULL, lwd = 2, pch = 17, type = "b", mai = c(0.6, 0.82, 0.2, 0.42), ...) {
  data.list <- .SCprepareData(data)
  
  annotations.cex <- 0.8 ### maybe for later implementation as an argument
  
  case.names <- names(data.list)
  
  if(class(lines) != "list")
    lines <- lapply(lines,function(x) x)
  
  
  if(!is.null(names(lines))) {
    id <- which(names(lines)=="")
    names(lines)[id] <- lines[id]
    lines[id] <- NA
  } else {
    tmp <- lines
    lines <- rep(NA, length(lines))
    names(lines) <- tmp
  }
  
  N <- length(data.list)
  if(N > 1) op <- par(mfrow = c(N, 1)) else op <- par(lwd = par()$lwd)
  
  values.tmp <- unlist(lapply(data.list, function(x) x[,2]))
  mt.tmp <- unlist(lapply(data.list, function(x) x[,3]))
  
  
  if (is.null(ylim))
    ylim <- c(min(values.tmp, na.rm = TRUE), max(values.tmp, na.rm = TRUE))
  if (is.null(xlim))
    xlim <- c(min(mt.tmp, na.rm = TRUE), max(mt.tmp, na.rm = TRUE))
  
  par(cex = 1)
  par(mex = 1)
  par(mgp = c(2,1,0))
  for(i in 1:N) {
    data <- data.list[[i]]
    data <- data[!is.na(data$values),] #maybe use the complete function later
    A <- data[,2][data[,1] == "A"]
    B <- data[,2][data[,1] == "B"]
    
    design <- rle(as.character(data$phase))
    design$start <- c(1,cumsum(design$lengths)+1)[1:length(design$lengths)]
    design$stop <- cumsum(design$lengths)
    class(design) <- "list"
    
    n1 <- design$lengths[1] #length(A)
    n2 <- design$lengths[2]
    
    y.lim <- ylim
    if(is.na(ylim[2]))
      y.lim[2] <- max(data[,2])
    if(is.na(ylim[1]))
      y.lim[1] <- min(data[,2])
    
    if (i == N) {
      par(mai = mai)
      plot(data[,3], data[,2], xlab = xlab, type = "n", xlim = xlim, ylim = y.lim, ylab = ylab, lwd = lwd, pch = pch, xaxp = c(xlim[1],xlim[2],xlim[2] - xlim[1]), ...)
    }
    else {
      if (i == 1)
        par(mai = c(0.2, 0.82, 0.6, 0.42))
      else  
        par(mai = c(0.4, 0.82, 0.4, 0.42))
      plot(data[,3], data[,2], xaxt = "n", xlab = "", lwd = lwd, type = "n", xlim = xlim, ylim = y.lim, ylab = ylab, pch = pch)
    }
      ################# DELETE when finished ###############
      Ax <- data[1:n1,3]
      Ax <- Ax[!is.na(A)]
      Bx <- data[(n1+1):(n1+n2),3]
      Bx <- Bx[!is.na(B)]
      A <- A[!is.na(A)]
      B <- B[!is.na(B)]
      ################# END: DELETE when finished ###############
    if(fill != "") {
      for(i in 1:length(design$values)) {
        x <- data$mt[design$start[i]:design$stop[i]]
        y <- data$values[design$start[i]:design$stop[i]]
        .SCfill(x, y, y.lim[1], fill)
      }
    }

    for(i in 1:length(design$values)) {
      x <- data$mt[design$start[i]:design$stop[i]]
      y <- data$values[design$start[i]:design$stop[i]]
      lines(x, y, type = type, pch = pch, lwd = lwd, ...)
    }

    if(!is.null(marks)) {
      marks.cex <- 1
      marks.col <- "red"
      marks.pch <- pch
      
      if (any(names(marks) == "positions")) {
        marks.pos <- marks[[which(names(marks) == "positions")]]
      } else {stop("Positions of marks must be defined.")}
      
      if (any(names(marks) == "cex")) {
        marks.cex <- marks[[which(names(marks) == "cex")]]
      }
      if (any(names(marks) == "col")) {
        marks.col <- marks[[which(names(marks) == "col")]]
      }
      if (any(names(marks) == "pch")) {
        marks.pch <- marks[[which(names(marks) == "pch")]]
      }
      
      if(class(marks.pos) == "numeric") {
        mks <- marks.pos
      } else {
        mks <- marks.pos[[i]]
      }
      marks.x <- data$mt[data$mt %in% mks]
      marks.y <- data$values[data$mt %in% mks]
      points(x = marks.x, y = marks.y, pch = marks.pch, cex = marks.cex, col = marks.col)
    }
    
    if(!is.null(annotations)) {
      annotations.cex <- 1
      annotations.round <- 1
      annotations.col <- "black"
      annotations.pos <- 3
      annotations.offset <- 0.5
      
      if (any(names(annotations) == "cex")) {
        annotations.cex <- annotations[[which(names(annotations) == "cex")]]
      }
      if (any(names(annotations) == "col")) {
        annotations.col <- annotations[[which(names(annotations) == "col")]]
      }
      if (any(names(annotations) == "round")) {
        annotations.round <- annotations[[which(names(annotations) == "round")]]
      }
      if (any(names(annotations) == "pos")) {
        annotations.pos <- annotations[[which(names(annotations) == "pos")]]
      }
      if (any(names(annotations) == "offset")) {
        annotations.offset <- annotations[[which(names(annotations) == "offset")]]
      }
      
      annotations.label <- round(data$values, annotations.round)
      ### not yet implemented
      if (any(names(annotations) == "label")) {
        id <- which(names(annotations) == "label")
        if(annotations[[id]]=="values") {
        } else {
        }
      }

      text(data$mt,data$values, label = annotations.label, col = annotations.col, pos = annotations.pos, offset = annotations.offset, cex = annotations.cex)
    }
    
    
    #### START: Adding help-lines
    label <- ""
    labelxy <- c(0,0)
    lty.line <- "dashed"
    lwd.line <- 2
    col.line <- "black"
    
    if (any(names(lines) == "lty")) {
      id <- which(names(lines) == "lty")
      lty.line <- lines[[id]]
    }
    if (any(names(lines) == "col")) {
      id <- which(names(lines) == "col")
      col.line <- lines[[id]]
    }
    if (any(names(lines) == "lwd")) {
      id <- which(names(lines) == "lwd")
      lwd.line <- lines[[id]]
    }
    if (any(names(lines) == "trend")) {
      reg <- lm(A~Ax)
      lines(c(min(Ax), max(Ax)), c(reg$coefficients[1] + min(Ax) * reg$coefficients[2], reg$coefficients[1] + max(Ax) * reg$coefficients[2]), lty = lty.line, col = col.line, lwd = lwd.line)
      reg <- lm(B~I(Bx-Bx[1]+1))
      lines(c(min(Bx), max(Bx)), c(reg$coefficients[1] + 1 * reg$coefficients[2], reg$coefficients[1] + (Bx[length(Bx)] - Bx[1]+ 1)  * reg$coefficients[2]), lty = lty.line, col = col.line, lwd = lwd.line)
    }
    if (any(names(lines) == "median")) {
      lines(c(min(Ax), max(Ax)), c(median(A, na.rm = TRUE), median(A, na.rm = TRUE)), lty = lty.line, col = col.line, lwd = lwd.line)		
      lines(c(min(Bx), max(Bx)), c(median(B, na.rm = TRUE), median(B, na.rm = TRUE)), lty = lty.line, col = col.line, lwd = lwd.line)		
      labelxy <- c(max(Bx), median(B,na.rm = TRUE))
      label <- "Median"
      
    }
    if (any(names(lines) == "mean")) {
      id <- which(names(lines) == "mean")
      lines.par <- lines[[id]]
      if (is.na(lines.par)) lines.par <- 0.1
      lines(c(min(Ax), max(Ax)), c(mean(A, trim = lines.par, na.rm = TRUE), mean(A, trim = lines.par, na.rm = TRUE)), lty = lty.line, col = col.line, lwd = lwd.line)		
      lines(c(min(Bx), max(Bx)), c(mean(B, trim = lines.par, na.rm = TRUE), mean(B, trim = lines.par, na.rm = TRUE)), lty = lty.line, col = col.line, lwd = lwd.line)		
      labelxy <- c(max(Bx), mean(B, trim = lines.par, na.rm = TRUE))
      label <- "Trimmed mean"
    }
    if (any(names(lines) == "trendA")) {
      #reg <- lm(A~I(1:length(A)))
      reg <- lm(A~Ax)
      lines(c(min(Ax), max(Bx)), c(reg$coefficients[1]  + min(Ax) * reg$coefficients[2], reg$coefficients[1] + max(Bx) * reg$coefficients[2]), lty = lty.line, col = col.line, lwd = lwd.line)
      labelxy <- c(max(Bx), reg$coefficients[1] + (max(Bx) - min(Bx)) * reg$coefficients[2])
      label <- "Trend A"
    }
    if (any(names(lines) == "loreg")) {
      id <- which(names(lines) == "loreg")
      lines.par <- lines[[id]]
      if (is.na(lines.par)) lines.par <- 0.5
      
      AB <- c(A,B)
      ABx <- c(Ax,Bx)
      reg <- lowess(AB~ABx, f = lines.par)
      lines(reg, lty = lty.line, col = col.line, lwd = lwd.line)
      labelxy <- c(max(Bx), (max(AB)-min(AB))/2+min(AB))
      label <- "Local Regression"
    }
    
    if (any(names(lines) == "pnd") || any(names(lines) == "maxA")) {
      lines(c(min(Ax), max(Bx)), c(max(A), max(A)), lty = lty.line, col = col.line, lwd = lwd.line)		
      labelxy <- c(max(Bx), max(A))
      label <- "Max A"
    }
    if (any(names(lines) == "medianA")) {
      lines(c(min(Ax), max(Bx)), c(median(A, na.rm = TRUE), median(A, na.rm = TRUE)), lty = lty.line, col = col.line, lwd = lwd.line)		
      labelxy <- c(max(Bx), median(A, na.rm = TRUE))
      label <- "Median A"
    }
    if (any(names(lines) == "meanA")) {
      id <- which(names(lines) == "meanA")
      lines.par <- lines[[id]]
      if (is.na(lines.par)) lines.par <- 0.1
      
      lines(c(min(Ax), max(Bx)), c(mean(A, trim = lines.par, na.rm = TRUE), mean(A, trim = lines.par, na.rm = TRUE)), lty = lty.line, col = col.line, lwd = lwd.line)		
      labelxy <- c(max(Bx), mean(A, trim = lines.par, na.rm = TRUE))
      label <- "Mean A"
    }
    if (any(names(lines) == "piecewisereg") || any(names(lines) == "plm")) {
      pr <- plm(data)
      y <- pr$full.model$fitted.values
      lines(data[,3], y, lty = lty.line, col = col.line, lwd = lwd.line)
    }
    if (any(names(lines) == "plm.ar")) {
      id <- which(names(lines) == "plm.ar")
      lines.par <- as.numeric(lines[[id]])
      pr <- plm(data, AR = lines.par)
      y <- pr$full.model$fitted
      lines(data[,3], y, lty = lty.line, col = col.line, lwd = lwd.line)
    }
    
    if (any(names(lines) == "movingMean")) {
      id <- which(names(lines) == "movingMean")
      lines.par <- lines[[id]]
      if (is.na(lines.par)) lines.par <- 1
      y <- .SCmovingAverage(c(A,B),lines.par, mean)
      lines(c(Ax,Bx), y, lty = lty.line, col = col.line, lwd = lwd.line)
    }
    if (any(names(lines) == "movingMedian")) {
      id <- which(names(lines) == "movingMedian")
      lines.par <- lines[[id]]
      if (is.na(lines.par)) lines.par <- 1
      y <- .SCmovingAverage(c(A,B),lines.par, median)
      lines(c(Ax,Bx), y, lty = lty.line, col = col.line, lwd = lwd.line)
    }
    
    if (!is.null(FUN.AB)){
      FUN <- FUN.AB
      .xA <- FUN(A)
      .xB <- FUN(B)
      lines(c(min(Ax), max(Ax)), c(.xA,.xA), lty = lty.line, col = col.line, lwd = lwd.line)
      lines(c(min(Bx), max(Bx)), c(.xB,.xB), lty = lty.line, col = col.line, lwd = lwd.line)
      labelxy <- c(max(Bx), .xB)
      #label <- FUN.AB[2]
    }
    
    text(labelxy[1], labelxy[2], label, adj = c(1,1))
    
    #### END: Adding help-lines
    
    ### Adding phase names
    if (is.null(phase.names))
      phase.names <- design$values
    for(i in 1:length(design$values)) {
      mtext(phase.names[i], side = 3, at = (data$mt[design$stop[i]] - data$mt[design$start[i]]) / 2 + data$mt[design$start[i]])
    }
    
    
    ### Adding vertical line between phases
    if(is.null(text.ABlag)) {
      for(i in 1:(length(design$values) - 1)) {
        abline(v = data$mt[design$stop[i]+1] - 0.5, lty = 2,lwd = lwd)
      }
      
    }
      
    if(!is.null(text.ABlag)) {
      for(i in 1:(length(design$values) - 1)) {
        tex <- paste(unlist(strsplit(text.ABlag[i], "")), collapse ="\n")
        text(data$mt[design$stop[i]+1] - 0.5, (y.lim[2]-y.lim[1])/2 + y.lim[1], labels = tex, cex = 1)
      }
      
         
    }
    
    ### Adding case name
    if (length(case.names) ==  N)
      mtext(case.names[i], side = 3, line = -1, adj = 0, at = 1)	
  }
  par(op)
}
