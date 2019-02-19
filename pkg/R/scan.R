.onAttach <- function(lib, pkg, ...) {
	out <- paste0("scan ",packageVersion("scan")," (development version, ",packageDate('scan'),")\n", #", packageDate("scan"), ")\n",
	              "Single-Case Data Analysis for Single and Multiple Baseline Designs\n")
	packageStartupMessage(out)
}	

.onLoad <- function(lib, pkg, ...) {}

#defaultAttributesSCDF <- function(...) {.defaultAttributesSCDF(...)}  

.opt <- list(
  dv    = "var.values",
  phase = "var.phase",
  mt    = "var.mt"
  )
  
.defaultAttributesSCDF <- function(attri = NULL) {
  out <- list()
  if(!is.null(attri))
    out <- attri
  out$class <- c("scdf","list")
  out[.opt$phase] <- "phase"
  out[.opt$dv] <- "values"
  out[.opt$mt] <- "mt"
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

.phasestructure <- function(data,pvar) {
  phases <- rle(as.character(data[,pvar]))
  phases$start <- c(1, cumsum(phases$lengths) + 1)[1 : length(phases$lengths)]
  phases$stop <- cumsum(phases$lengths)
  class(phases) <- "list"
  return(phases)
}

.SCprepareData <- function(data, na.rm = FALSE, change.var.phase = TRUE, change.var.values = TRUE, change.var.mt = TRUE) {
  
  if(is.data.frame(data)) {
    data <- list(data)
    attributes(data) <- .defaultAttributesSCDF()
  }
  if(!is.list(data))
    stop("Wrong data format. Data must be a data frame or a list of data frames.")
  
  if(is.null(attr(data, .opt$phase)))
    attr(data,.opt$phase) <- "phase"
  if(is.null(attr(data, .opt$mt)))
    attr(data,.opt$mt)    <- "mt"
  if(is.null(attr(data, .opt$dv)))
    attr(data,.opt$dv)    <- "values"
  
  pvar       <- attr(data,.opt$phase)
  var.mt     <- attr(data, .opt$mt)
  var.values <- attr(data, .opt$dv)

  
  if(is.null(names(data)))
    names(data) <- paste0("Case",1:length(data))
  
  for(case in 1:length(data)) {
    VARS <- names(data[[case]])
    if(!(var.values %in% VARS))
      stop("No variable for values with the name ",var.values, " in the scdf.")
    if(!(pvar %in% VARS))
      stop("No variable for phase with the name ",pvar,  " in the scdf.")
    if(!(var.mt %in% VARS))
      stop("No variable for mt with the name ",var.mt,     " in the scdf.")
   
    if(na.rm)
      data[[case]] <- data[[case]][!is.na(data[[case]][, var.values]),]
    if(!is.factor(data[[case]][, pvar]))
      data[[case]][, pvar] <- as.factor(data[[case]][, pvar])
    
    
    if(change.var.values && var.values != "values") {
      if("values" %in% VARS) {
        warning("Original values variable was renamed to values_renamed for this analysis.")
        names(data[[case]])[match("values",VARS)] <- "values_renamed"
      }
      names(data[[case]])[match(var.values, VARS)] <- "values"
    }
    
    if(change.var.mt && !(var.mt %in% VARS)) {
      data[[case]][,var.mt] <- 1:nrow(data[[case]])
    }
    
    if(change.var.mt && var.mt != "mt") {
      if("mt" %in% VARS) {
        warning("Original mt variable was renamed to mt_renamed for this analysis.")
        names(data[[case]])[match("mt",VARS)] <- "mt_renamed"
      }
      names(data[[case]])[match(var.mt, VARS)] <- "mt"
    }
    
    if(change.var.phase && pvar != "phase") {
      if("phase" %in% VARS) {
        warning("Original phase variable was renamed to phase_renamed for this analysis.")
        names(data[[case]])[match("phase",VARS)] <- "phase_renamed"
      }
      names(data[[case]])[match(pvar, VARS)] <- "phase"
    }
    if(is.na(names(data)[case]))
      names(data)[case] <- paste0("Case ", case)
  }
  
  
  
  return(data)
}

#keepphasesSC <- function(...) {.keepphasesSC(...)}

.keepphasesSC <- function(data, phases = c(1,2), set.phases = TRUE, pvar = "phase") {
  
  if(is.data.frame(data))
    data <- list(data)
  
  ATTRIBUTES <- attributes(data)
  
  res <- lapply(data, function(x) rle(as.character(x[,pvar]))$values)
  if(!all(unlist(lapply(res[-1], function(x) identical(x,res[[1]])))))
    warning("Single-cases do have differing desings.")
  
  if (class(phases) %in% c("character","numeric","integer")) {
    if(!length(phases) == 2) 
      stop("Phases argument not set correctly. Please provide a vector with two charcters or two numbers. E.g., phases = c(1,3).")
    phases.A <- phases[1]
    phases.B <- phases[2]
  }
  
  if (class(phases) == "list") {
    phases.A <- phases[[1]]
    phases.B <- phases[[2]]
  }
  
  phases.total <- c(phases.A, phases.B)
  design <- rle(as.character(data[[1]][,pvar]))
  
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
    design <- rle(as.character(data[[case]][,pvar]))
    design$start <- c(1,cumsum(design$lengths)+1)[1:length(design$lengths)]
    design$stop <- cumsum(design$lengths)
    class(design) <- "list"
    
    A <- unlist(lapply(phases.A, function(x) design$start[x]:design$stop[x]))
    B <- unlist(lapply(phases.B, function(x) design$start[x]:design$stop[x]))
    
    data[[case]][,pvar] <- as.character(data[[case]][,pvar])
    
    if(set.phases) {
      data[[case]][A ,pvar] <- "A"
      data[[case]][B ,pvar] <- "B"
    }
    data[[case]] <- data[[case]][c(A,B),]
    design.list[[case]] <- design
  }
  attributes(data) <- ATTRIBUTES
  out <- list(data = data, designs = design.list, N = N, phases.A = phases.A, phases.B = phases.B)
  return(out)
}


.onAttach()



