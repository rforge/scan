.SCprepareData <- function(data, na.rm = FALSE, change.var.phase = TRUE, change.var.values = TRUE, change.var.mt = TRUE) {
  
  if (is.data.frame(data)) {
    data <- list(data)
    attributes(data) <- .defaultAttributesSCDF()
  }
  
  if (!is.list(data)) {
    stop("Wrong data format. Data must be a data frame or a list of data frames.")
  }
  
  if (is.null(scdf_attr(data, .opt$phase))) scdf_attr(data, .opt$phase) <- "phase"
  if (is.null(scdf_attr(data, .opt$mt))) scdf_attr(data, .opt$mt) <- "mt"
  if (is.null(scdf_attr(data, .opt$dv))) scdf_attr(data, .opt$dv) <- "values"
  
  pvar       <- scdf_attr(data, .opt$phase)
  var.mt     <- scdf_attr(data, .opt$mt)
  var.values <- scdf_attr(data, .opt$dv)
  
  names(data) <- .case.names(names(data), length(data))
  
  for(case in 1:length(data)) {
    VARS <- names(data[[case]])
    if (!all(var.values %in% VARS)){
      stop("No variable for values with the name ", var.values, " in the scdf.")
    }
    if (!(pvar %in% VARS)) {
      stop("No variable for phase with the name ", pvar, " in the scdf.")
    }
    if (!(var.mt %in% VARS)) {
      stop("No variable for mt with the name ",var.mt, " in the scdf.")
    }
    if (na.rm) data[[case]] <- data[[case]][!is.na(data[[case]][, var.values]), ]
    if (!is.factor(data[[case]][, pvar])) data[[case]][, pvar] <- as.factor(data[[case]][, pvar])
    
    if (change.var.values && var.values != "values") {
      if ("values" %in% VARS) {
        warning("Original values variable was renamed to values_renamed for this analysis.")
        names(data[[case]])[match("values", VARS)] <- "values_renamed"
      }
      names(data[[case]])[match(var.values, VARS)] <- "values"
    }
    
    if (change.var.mt && !(var.mt %in% VARS)) {
      data[[case]][,var.mt] <- 1:nrow(data[[case]])
    }
    
    if (change.var.mt && var.mt != "mt") {
      if ("mt" %in% VARS) {
        warning("Original mt variable was renamed to mt_renamed for this analysis.")
        names(data[[case]])[match("mt", VARS)] <- "mt_renamed"
      }
      names(data[[case]])[match(var.mt, VARS)] <- "mt"
    }
    
    if (change.var.phase && pvar != "phase") {
      if ("phase" %in% VARS) {
        warning("Original phase variable was renamed to phase_renamed for this analysis.")
        names(data[[case]])[match("phase", VARS)] <- "phase_renamed"
      }
      names(data[[case]])[match(pvar, VARS)] <- "phase"
    }
    if (is.na(names(data)[case]))
      names(data)[case] <- paste0("Case ", case)
  }
  data
}


depracted_.SCprepareData <- function(data, na.rm = FALSE, change.var.phase = TRUE, change.var.values = TRUE, change.var.mt = TRUE) {
  
  if (is.data.frame(data)) {
    data <- list(data)
    attributes(data) <- .defaultAttributesSCDF()
  }
  
  if (!is.list(data)) {
    stop("Wrong data format. Data must be a data frame or a list of data frames.")
  }
  
  if (is.null(attr(data, .opt$phase))) attr(data, .opt$phase) <- "phase"
  if (is.null(attr(data, .opt$mt))) attr(data, .opt$mt) <- "mt"
  if (is.null(attr(data, .opt$dv))) attr(data, .opt$dv) <- "values"
  
  pvar       <- attr(data, .opt$phase)
  var.mt     <- attr(data, .opt$mt)
  var.values <- attr(data, .opt$dv)
  
  names(data) <- .case.names(names(data), length(data))
  
  for(case in 1:length(data)) {
    VARS <- names(data[[case]])
    if (!all(var.values %in% VARS)){
      stop("No variable for values with the name ", var.values, " in the scdf.")
    }
    if (!(pvar %in% VARS)) {
      stop("No variable for phase with the name ", pvar, " in the scdf.")
    }
    if (!(var.mt %in% VARS)) {
      stop("No variable for mt with the name ",var.mt, " in the scdf.")
    }
    if (na.rm) data[[case]] <- data[[case]][!is.na(data[[case]][, var.values]), ]
    if (!is.factor(data[[case]][, pvar])) data[[case]][, pvar] <- as.factor(data[[case]][, pvar])
    
    if (change.var.values && var.values != "values") {
      if ("values" %in% VARS) {
        warning("Original values variable was renamed to values_renamed for this analysis.")
        names(data[[case]])[match("values", VARS)] <- "values_renamed"
      }
      names(data[[case]])[match(var.values, VARS)] <- "values"
    }
    
    if (change.var.mt && !(var.mt %in% VARS)) {
      data[[case]][,var.mt] <- 1:nrow(data[[case]])
    }
    
    if (change.var.mt && var.mt != "mt") {
      if ("mt" %in% VARS) {
        warning("Original mt variable was renamed to mt_renamed for this analysis.")
        names(data[[case]])[match("mt", VARS)] <- "mt_renamed"
      }
      names(data[[case]])[match(var.mt, VARS)] <- "mt"
    }
    
    if (change.var.phase && pvar != "phase") {
      if ("phase" %in% VARS) {
        warning("Original phase variable was renamed to phase_renamed for this analysis.")
        names(data[[case]])[match("phase", VARS)] <- "phase_renamed"
      }
      names(data[[case]])[match(pvar, VARS)] <- "phase"
    }
    if (is.na(names(data)[case]))
      names(data)[case] <- paste0("Case ", case)
  }
  data
}
