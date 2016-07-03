
makeSCDF <- function (data, B.start = NULL, MT = NULL){
  out <- .SCprepareData(data = data, B.start = B.start, MT = MT)
  if(length(out) == 1)
    return(out[[1]])
  return(out)
}
