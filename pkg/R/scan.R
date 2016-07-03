
.onAttach <- function(lib, pkg, ...) {
	out <- paste0("scan ",packageVersion("scan"),"\n","Single-Case Data Analysis for Single and Multiple AB-Designs\n",
	              "Caution! This is a beta version and heavily under construction!\n")
	packageStartupMessage(out)
}	

.onLoad <- function(lib, pkg, ...) {}

.SCmovingAverage <- function(x, xLag, FUN = mean) {
	for(i in (xLag + 1):(length(x) - xLag))
		x[i] <- FUN(x[(i - xLag):(i + xLag)], na.rm = TRUE)
	return(x)
}
	
.SCac <- function(x, lag = 1) {
	m <- mean(x, na.rm = TRUE)
	ax1 <- x[1:(length(x) - lag)]-m
	ax2 <- x[(lag + 1):length(x)]-m
	ac <- sum(ax1*ax2, na.rm = TRUE)/sum((x-m)^2, na.rm = TRUE)
	ac
}

.SClm <- function(x = NULL,y) {
	if(is.null(x))
		x <- 1:length(y)
	mx <- mean(x)
	my <- mean(y)
	ss.xy <- sum( (x-mx)*(y-my) )
	ss.xx <- sum( (x-mx)^2 )
	b <- ss.xy/ss.xx
	b
}

.SCbeta <- function(model) {
	  b <- model$coefficients[-1]
    sx <- apply(model$model[-1],2,sd)
    sy <- apply(model$model[1],2,sd)
    return(c(model$coefficients,b * sx/sy))
}

.SCprepareData <- function(data, B.start = NULL, MT = NULL) {
	
	#vector with B.start -> single data frame

	if (class(data) == "numeric" || class(data) == "integer") {
		if (is.null(MT))
			MT <- 1:length(data)
		B.start <- which(MT == B.start)
		D <- c(rep("A", B.start - 1), rep("B", length(data) - B.start + 1))
		data <- data.frame(phase = D, values = data, mt = MT)
	}
	
	#single data.frame -> list with single data frame
	if (class(data) == "data.frame") {
		if(ncol(data) == 2)
			data[3] <- 1:nrow(data)
		names(data) <- c("phase", "values", "mt")
		data <- list(data)
	}
	
	#list with vectors and vector with B.start -> list with data frames
	if(length(B.start) > 1) {
		if(length(data) != length(B.start) || mode(data) != "list")
			stop("Wrong data format. Format schould be a list of vectors and a vector of startpoints.")
		dat <- list()
		for(i in 1:length(data)) {
			if (is.null(MT))
				MT.tmp <- 1:length(D)
			else
				MT.tmp <- MT[[i]]
			B.start[i] <- which(MT.tmp == B.start[i])
			D <- c(rep("A", B.start[i] - 1), rep("B", length(data[[i]]) - B.start[i] + 1))
			dat[[i]] <- data.frame(phase = D, values = data[[i]], mt = MT.tmp)
		}
		data <- dat
	}
	
	if(class(data) != "list")
		stop("Wrong data format. Data have to be a numeric vector, a data.frame, or a list.")


	for(i in 1:length(data))
		if(ncol(data[[i]]) == 2) data[[i]]$mt <- 1:nrow(data[[i]])
		
	return(data)
}

makeSCDF <- function (data, B.start = NULL, MT = NULL){
	out <- .SCprepareData(data = data, B.start = B.start, MT = MT)
  if(length(out) == 1)
    return(out[[1]])
	return(out)
}

longSCDF <- function(data) {
  dat <- .SCprepareData(data)
  label <- names(dat)
  if (is.null(label))
    label <- as.character(1:length(dat))
  outdat <- vector()
  for (i in 1:length(dat)) {
	  dat[[i]]$case <- label[i]
		outdat <- rbind(outdat, dat[[i]])
	}
  outdat <- cbind(outdat[,ncol(outdat)],outdat[,-ncol(outdat)])
  colnames(outdat)[1] <- "case"
  return(outdat)
}


writeSC <- function(dat, filename, sep = ",", dec = ".", ...) {
	write.table(longSCDF(dat), filename, sep = sep, row.names = FALSE, dec = dec, ...)
}




.onAttach()



