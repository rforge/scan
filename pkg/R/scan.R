
.onAttach <- function(lib, pkg, ...) {
	out <- paste0("scan ",packageVersion("scan"),"\n","Single-Case Data Analysis for Single and Multiple Baseline Designs\n",
	              "Caution! This is a beta version and heavily under construction!\n")
	packageStartupMessage(out)
}	

.onLoad <- function(lib, pkg, ...) {}

.defaultAttributesSCDF <- function(attri = NULL) {
  out <- list()
  if(!is.null(attri))
    out <- attri
  out$class <- c("scdf","list")
  out$phase <- "phase"
  out$values <- "values"
  out$mt <- "mt"

  out
}  

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

.SCprepareData <- function(data, na.rm = FALSE) {
	
	if(is.data.frame(data)) {
		data <- list(data)
		attributes(data) <- .defaultAttributesSCDF()
  }
	if(!is.list(data))
		stop("Wrong data format. Data must be a data frame or a list of data frames.")
  
  if(is.null(attributes(data)$phase))
    attr(data,"phase") <- "phase"
  if(is.null(attributes(data)$mt))
    attr(data,"mt") <- "mt"
  if(is.null(attributes(data)$values))
    attr(data,"values") <- "values"
  
	var.phase <- attributes(data)$phase
	var.mt <- attributes(data)$mt
	var.values <- attributes(data)$values
	
	for(case in 1:length(data)) {
	  VARS <- names(data[[case]])
		if(is.na(var.values %in% VARS))
	    stop("No variable with the name ",var.values," in the scdf.")
	  if(is.na(var.phase %in% VARS))
	    stop("No variable with the name ",var.phase," in the scdf.")
	  if(is.na(var.mt %in% VARS))
	    stop("No variable with the name ",var.mt," in the scdf.")
	  
    if(var.values != "values") {
	    if(!is.na("values" %in% VARS)) {
	      warning("Original values variable was renamed.")
	      names(data[[case]])[match("values",VARS)] <- "values_renamed"
	    }
	    names(data[[case]])[match(var.values, VARS)] <- "values"
    }

	  
	  if(!(var.mt %in% VARS)) {
	    data[[case]][,var.mt] <- 1:nrow(data[[case]])
	  }
	  
	  if(var.mt != "mt") {
	    if(!is.na("mt" %in% VARS)) {
	      warning("Original mt variable was renamed.")
	      names(data[[case]])[match("mt",VARS)] <- "mt_renamed"
	    }
	    names(data[[case]])[match(var.mt, VARS)] <- "mt"
	  }
	  
	  
	  if(!is.factor(data[[case]]$phase))
	    data[[case]]$phase <- as.factor(data[[case]]$phase)
    if(na.rm)
	    data[[case]] <- data[[case]][!is.na(data[[case]]$values),]
  }

	return(data)
}

keepphasesSC <- function(data, phases = c("A","B"), set.phases = TRUE) {

  
  if(is.data.frame(data))
    data <- list(data)
  res <- lapply(data, function(x) rle(as.character(x$phase))$values)
  if(!all(unlist(lapply(res[-1], function(x) identical(x,res[[1]])))))
    warning("Single-cases do have differing desings.")

  if (class(phases) == "character" || class(phases) == "numeric") {
    if(!length(phases) == 2) 
      stop("Phases argument not correctly set. Please provide a vector with two charcters or two numbers. E.g., phases = c(1,3).")
    phases.A <- phases[1]
    phases.B <- phases[2]
  }

  if (class(phases) == "list") {
    phases.A <- phases[[1]]
    phases.B <- phases[[2]]
  }

  phases.total <- c(phases.A, phases.B)
  design <- rle(as.character(data[[1]]$phase))
  
  if(class(phases.total) == "character") {
    tmp <- sapply(phases.total, function(x) sum(x == design$values)>1)
    if(any(tmp))
      stop(paste0("Phase names ", paste0(names(tmp[tmp]))," occure several times. Please give number of phases instead of characters."))
    
    tmp <- sapply(phases.total, function(x) any(x == design$values))
    if(!all(tmp))
      stop(paste0("Phase names ",  names(tmp[!tmp]) ," do not occure in the data. Please give different phase names."))
  }

  if(class(phases.total) == "character") {
    phases.A <- which(design$values %in% phases.A)
    phases.B <- which(design$values %in% phases.B)
  }
  
  N <- length(data)
  design.list <- list()
  
  for(case in 1:N) {
    design <- rle(as.character(data[[case]]$phase))
    design$start <- c(1,cumsum(design$lengths)+1)[1:length(design$lengths)]
    design$stop <- cumsum(design$lengths)
    class(design) <- "list"

    A <- unlist(lapply(phases.A, function(x) design$start[x]:design$stop[x]))
    B <- unlist(lapply(phases.B, function(x) design$start[x]:design$stop[x]))

    data[[case]]$phase <- as.character(data[[case]]$phase)
    
    if(set.phases) {
      data[[case]]$phase[A] <- "A"
      data[[case]]$phase[B] <- "B"
    }
    data[[case]] <- data[[case]][c(A,B),]
    design.list[[case]] <- design
  }
  
  out <- list(data = data, designs = design.list, N = N, phases.A = phases.A, phases.B = phases.B)
  return(out)
}


longSCDF <- function(data, l2 = NULL, id = "case", model = NULL, ...) {
  dat <- .SCprepareData(data)
  label <- names(dat)
  if (is.null(label))
    label <- as.character(1:length(dat))
  outdat <- vector()
  
  
  if(!is.null(model)) {
    for(case in 1:length(dat)) {
      data.inter <- plm.predictor(dat[[case]], model = model)
      dat[[case]]$mt <- data.inter$mt
      dat[[case]] <- cbind(dat[[case]],data.inter[,-1])
    }
  }
  
  
  for (case in 1:length(dat)) {
	  dat[[case]]$case <- label[case]
		outdat <- rbind(outdat, dat[[case]])
  }
  
  outdat <- cbind(outdat[,ncol(outdat)],outdat[,-ncol(outdat)])
  colnames(outdat)[1] <- "case"
  

  
  if(!is.null(l2)) {
    outdat <- merge(outdat, l2, by = id, ...)
    
    
  }
  return(outdat)
}



.onAttach()



