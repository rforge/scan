
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

makesingleSC <- function(data, scale = FALSE, type = "add") {
	warning("This function is deprecated. Please don't use it anymore.\n")
  data <- .SCprepareData(data)
	N <- length(data)
	for(i in 1:N) {
		m  <- mean(data[[i]][data[[i]][,1] == "A",2], na.rm = TRUE)
		if (!scale) 
			sd <- 1
		if (scale) 
			sd <-  sd(data[[i]][data[[i]][,1] == "A",2], na.rm = TRUE)
		data[[i]][,2] <- (data[[i]][,2] - m) / sd 
		A <- data[[i]][data[[i]][,1] == "A",]	
		B <- data[[i]][data[[i]][,1] == "B",]	
		B[,3] <- B[,3] - min(B[,3], na.rm = TRUE) + 1
		if(i == 1) {
			new.data.A <- A
			new.data.B <- B	
		}
		if(i > 1) {
			new.data.A <- rbind(new.data.A, A)	
			new.data.B <- rbind(new.data.B, B)	
		}
	}
	new.data.A <- new.data.A[order(new.data.A[,3]),]
	if(type == "mean") {
		tmp <- aggregate(values~mt, data = new.data.A, mean, na.rm = TRUE)
		new.data.A <- data.frame(phase = rep("A", nrow(tmp)), values = tmp$values, mt = tmp$mt)
		tmp <- aggregate(values~mt, data = new.data.B, mean, na.rm = TRUE)
		new.data.B <- data.frame(phase = rep("B", nrow(tmp)), values = tmp$values, mt = tmp$mt)
	}
	if(type == "median") {
		tmp <- aggregate(values~mt, data = new.data.A, median, na.rm = TRUE)
		new.data.A <- data.frame(phase = rep("A", nrow(tmp)), values = tmp$values, mt = tmp$mt)
		tmp <- aggregate(values~mt, data = new.data.B, median, na.rm = TRUE)
		new.data.B <- data.frame(phase = rep("B", nrow(tmp)), values = tmp$values, mt = tmp$mt)
	}
	maxA <- max(new.data.A[,3], na.rm = TRUE)
	new.data.B[,3] <- new.data.B[,3] + maxA
	new.data.B <- new.data.B[order(new.data.B[,3]),]
	new.data <- rbind(new.data.A, new.data.B)
		
	return(list(new.data))
}

truncateSC <- function (data, A = c(0,0), B = c(0,0)){
  data <- .SCprepareData(data)
  N = length(data)

  for(i in 1:N){
    lengthA <- sum(data[[i]]$phase == "A")
    lengthB <- sum(data[[i]]$phase == "B")
    selectA <- (A[1] + 1):(lengthA - A[2])
    select <- c(selectA,(lengthA + 1 + B[1]):(lengthA + lengthB - B[2]))
    data[[i]] <- data[[i]][select,]
    data[[i]]$mt <- 1:nrow(data[[i]])
  }
  return(data)
}

fillmissingSC <- function(data, interpolation = "linear", na.rm = TRUE) {
		data <- .SCprepareData(data)
		N <- length(data)
		for(i in 1:N) {
			dat <- data[[i]]
			if (na.rm)
			  dat <- dat[!is.na(dat$values),]
			new.dat <- dat
			for(j in 1 : (nrow(dat)-1)) {
				if(dat$mt[j+1] - dat$mt[j] != 1){
					if(interpolation == "linear")
						step.size <- (dat$values[j+1] - dat$values[j]) / (dat$mt[j+1] - dat$mt[j])
					for(k in (dat$mt[j]+1) : (dat$mt[j+1]-1)) {
						tmp <- dat[j, ]
						tmp$mt <- k
						if(interpolation == "linear")
							tmp$values <- dat$values[j] + step.size * (k - dat$mt[j])
						new.dat <- rbind(new.dat, tmp) 
					}
				}
			}
			data[[i]] <- new.dat[order(new.dat$mt),]
		}
	data
}


smoothSC <- function(data, FUN = "movingMedian", intensity = NULL){
	data <- .SCprepareData(data)
	if (FUN == "movingMean") {
		if(is.null(intensity)) intensity <- 1
		return(lapply(data, function(x) {
				x[,2] <- .SCmovingAverage(x[,2], intensity, mean)
				x}))
	}
	if (FUN == "movingMedian") {
		if(is.null(intensity)) intensity <- 1
		return(lapply(data, function(x) {
				x[,2] <- .SCmovingAverage(x[,2], intensity, median)
				x}))
	}
	if (FUN == "localRegression") {
		if(is.null(intensity)) intensity <- 0.2
		return(lapply(data, function(x) {
				xval <- x[,3]
				xval <- xval[!is.na(x[,2])]
				yval <- x[!is.na(x[,2]),2]
				x[,2] <- lowess(yval~xval, f = intensity)$y
				x}))
	}
}	


outlierSC <- function(data, criteria = c("SD", "2")){

  data.list <- .SCprepareData(data)
	
  out <- list()
  
  N <- length(data.list)
	case.names <- names(data.list)
  dropped.mts <- list()
  dropped.n <- list()
  ci.matrix <- list()
  sd.matrix <- list()
  cook <- list()

  if(is.null(case.names))
		case.names <- paste("Person", 1:N, sep = "")

	for(i in 1:N) {
		data <- data.list[[i]]
		A <- data[,2][data[,1] == "A"]
		B <- data[,2][data[,1] == "B"]
		if (criteria[1] == "CI") {
			cut.off <- as.numeric(criteria[2])
			fac <- qnorm((1-cut.off)/2, lower.tail = FALSE)
			critA.upper <- mean(A) + fac * (sd(A)/sqrt(length(A)))
			critA.lower <- mean(A) - fac * (sd(A)/sqrt(length(A)))
			critB.upper <- mean(B) + fac * (sd(B)/sqrt(length(B)))
			critB.lower <- mean(B) - fac * (sd(B)/sqrt(length(B)))
			filterA <- (A < critA.lower) | (A > critA.upper)
			filterB <- (B < critB.lower) | (B > critB.upper)
			filterAB <- c(filterA, filterB)
			mat <- matrix(c(critA.lower, critB.lower, critA.upper, critB.upper), nrow = 2, dimnames = list(c("A-phase","B-phase"), c("lower", "upper")))
			ci.matrix[[i]] <- mat
		}
		if (criteria[1] == "SD") {
			SD <- as.numeric(criteria[2])
			critA.upper <- mean(A) + SD * sd(A)
			critA.lower <- mean(A) - SD * sd(A)
			critB.upper <- mean(B) + SD * sd(B)
			critB.lower <- mean(B) - SD * sd(B)
			filterA <- (A < critA.lower) | (A > critA.upper)
			filterB <- (B < critB.lower) | (B > critB.upper)
			filterAB <- c(filterA, filterB)
			mat <- matrix(c(critA.lower, critB.lower, critA.upper, critB.upper), nrow = 2, dimnames = list(c("A-phase","B-phase"), c("lower", "upper")))
			sd.matrix[[i]] <- mat
		}		
		if (criteria[1] == "Cook") {
			if (criteria[2] == "4/n")
				cut.off <- 4/(length(A)+length(B))
			else
				cut.off <- as.numeric(criteria[2])
			n1 <- length(A)
			MT <- data[,3]
			values <- data[,2]
			T <- MT[n1+1]
			D <- c(rep(0, length(A)), rep(1, length(B)))
			int <-  D * (MT - T)
			reg <- lm(values ~ 1 + MT + D + int)
			cd <- cooks.distance(reg)
			filterAB <- cd >= cut.off
			cook[[i]] <- data.frame(Cook = round(cd,2), MT = MT)
		}		
		    
		#data.list[[i]][,4] <- filterAB
		#names(data.list[[i]])[4] <- "outlier"
		dropped.mts[[i]] <- data.list[[i]]$mt[filterAB]
		dropped.n[[i]] <- sum(filterAB)
    
    data.list[[i]] <- data.list[[i]][!filterAB,]
	}
  
  out$data <- data.list
  out$dropped.mt <- dropped.mts
  out$dropped.n <- dropped.n
  out$ci.matrix <- ci.matrix
  out$sd.matrix <- sd.matrix
  out$cook <- cook
  out$criteria <- criteria
  out$N <- N
  out$case.names <- case.names
  class(out) <- c("sc","outlier")
	out
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

readSC <- function(filename, sep = ",", dec = ".", sort.labels = FALSE, phase.names = c("A","B"),...) {
	dat <- read.table(filename, header = TRUE, sep = sep, dec = dec, stringsAsFactors = FALSE, ...)
	columns <- ncol(dat)
	names(dat) <- c("case", "phase", "values", "mt")[1:columns]
	if(!sort.labels) 
		dat$case <- factor(dat$case, levels = unique(dat$case))
	else
		dat$case <- factor(dat$case)
	
	dat$phase[dat$phase == phase.names[1]] <- "A"
	dat$phase[dat$phase == phase.names[2]] <- "B"
	dat$phase <- factor(dat$phase)
	
	lab <- levels(dat$case)
	dat <- split(dat, dat$case)
	dat <- lapply(dat, function(x) x[,2:columns])
	for(i in 1:length(dat))
		row.names(dat[[i]]) <- 1:nrow(dat[[i]])
	names(dat) <- lab
	cat("Imported",length(dat),"cases.\n")
	if(columns == 3) {
		cat("Measurement-times are missing. Standard times were assigned.\n")
		dat <- .SCprepareData(dat)
	}
	return(dat)
}



.onAttach()



