#' Create styles for single-case data plots
#' 
#' The \code{style.plotSC} function is used to create graphical styles for a single-case plot
#' 
#' 
#' @aliases style.plotSC
#' 
#' @param style Predefined styles. Posible values are: "default", "grid", "grid2", "nodots", "annotate", "dark", "dark2", "sienna"
#' @param ... Further arguments passed to the plot command.
#' 
#' @return Returns a list to be provided or the style argument of the plot function.
#' @return \itemize{ 
#' \item\code{fill} If set, the area under the line is
#' filled with the given color (e.g., \code{fill = "tomato"}). Use the standard
#' R command colors() to get a list of all possible colours. \code{fill} is
#' empty by default.  
#' \item\code{annotations} A list of parameters defining
#' annotations to each data point. This adds the score of each MT to your plot.
#' \itemize{ 
#' \item\code{"pos"} Position of the annotations: 1 = below, 2 =
#' left, 3 = above, 4 = right.  
#' \item\code{"col"} Color of the annotations.
#' \item\code{"cex"} Size of the annotations.
#' \item\code{"round"} Rounds the values to the specified decimal.} 
#' \item\code{annotations = list(pos = 3, col =
#' "brown", round = 1)} adds scores rounded to one decimal above the data point
#' in brown color to the plot.
#' \item\code{"lwd"} Width of the plot line. Default is \code{lwd = 2}.
#' \item\code{"pch"} Point type. Default is \code{pch = 17} (triangles). Other options
#' are for example: 16 (filled circles) or "A" (uses the letter A).
#' \item\code{"main"} Main title of the plot.
#' \item\code{"mai"} Sets the margins of the plot.
#' \item\code{"bty"} Shape of the frame surrounding the inner plot
#' \item\code{"fill.bg"} Backgroundcolor of the plot.
#' \item\code{"grid"} Color of a grid.
#' \item\code{"text.ABlag"} Text displayed between phases.
#' \item\code{"cex.axis"} Size of the axis annotations
#' \item\code{"las"} Orientation of the axis annotations
#' \item\code{"col.lines"} Color of the lines
#' \item\code{"col.dots"} Color of the dots
#' \item\code{"col.seperator"} Color of the phase seperating lines
#' \item\code{"col.bg"} Color of the outer plot
#' \item\code{"col"} General color setting for the plot
#' \item\code{"col.text"} Color of all labels of the plot.
#' }
#' @author Juergen Wilbert
#' @seealso \code{\link{plot.scdf}}
#' @examples
#' newstyle <- style.plotSC(style = "default")
#' newstyle$text.ABlag <- c("START","END")
#' newstyle$col.dots <- ""
#' newstyle$annotations <- list(cex = 0.6, col = "grey10", offset = 0.4)
#' plot(exampleABC, style = newstyle)

style.plotSC <- function(style = "default", ...) {
  new <- list(...)
  default <- list(frame = "black", fill = "", fill.bg = NA, grid = NA, annotations = NULL, 
                  text.ABlag = NULL, lwd = 2, pch = 17, font = "sans", 
                  cex = 1, cex.axis = 0.8, cex.text = 1, cex.lab = 1,
                  las = 1, mai = c(0.6, 0.82, 0.2, 0.42), bty = "o", 
                  col.lines = "black", col.dots = "black", col.seperators = "black", 
                  col.bg = "white", col = "black", col.text = "black" 
                  )
  if(style == "default")
    out <- list()
  if(style == "annotate")
    out <- list(annotations = list(cex = 0.6, col = "blue", offset = 0.4), pch = 19)
  if(style == "grid")
    out <- list(frame = NA, grid = "lightblue", fill.bg = "grey95", lwd = 0.7, cex = 1, pch = 19, las = 1, cex.axis = 0.8, las = 1)  
  if(style == "grid2")
    out <- list(frame = NA, fill = "white", grid = "lightgreen", frame = "black", fill.bg = "grey95", lwd = 0.7, cex = 1, pch =1, las = 1, cex.axis = 0.8, las = 1)  
  if(style == "dark")
    out <- list(fill.bg = "black", bty = "o", col.lines = "gold", col.bg = "grey10", col.dots = "red", col.seperators = "white", col = "white", col.text = "white")
  if(style == "dark2")
    out <- list(fill = "grey30", fill.bg = "black", grid = "white", lwd = 2, pch = 1, bty = "o", col.lines = "gold", col.dots = "red", col.seperators = "white", col = "white", col.bg = "grey10", col.text = "white", font = "mono")
  if(style == "nodots")
    out <- list(type = "l", col.dots = "", fill = "grey95", grid = "grey80", las = 1, fill.bg = "grey99")
  if(style == "sienna")
    out <- list(grid = "orange", pch = 18, col.lines = "grey85", col.dots = "seagreen4", lwd = 2, col.bg = "seashell", fill.bg = "moccasin", col.text = "sienna4", col = "darkolivegreen", col.seperators = "sienna4", las = 1, cex = 1, cex.text = 0.8, cex.lab = 0.8, cex.axis = 0.7, frame = "darkseagreen", font = "serif")
  out <- c(out, default)
  out <- out[unique(names(out))]
  if(is.list(new)) {
    out <- c(new, out)
    out <- out[unique(names(out))]
  }
  out
}
