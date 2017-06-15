
newmakeSCDF <- function (data, B.start = NULL, MT = NULL, phase.length = NULL){
  if (is.null(MT))
    MT <- 1:length(data)
  if(!is.null(B.start)) {
    B.start <- which(MT == B.start)
    D <- c(rep("A", B.start - 1), rep("B", length(data) - B.start + 1))
  }
  if(!is.null(phase.length))
      D <- rep(names(phase.length),phase.length)

  data <- data.frame(phase = D, values = data, mt = MT)
  data
}
