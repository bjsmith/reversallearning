#' Connect control points/observations with an X-spline
#'
#' Draw an X-spline, a curve drawn relative to control points/observations.
#' Patterned after \code{geom_line} in that it orders the points by \code{x}
#' first before computing the splines.
#'
#' \if{html}{
#' A sample of the output from \code{geom_myhdi()}:
#'
#' \figure{geommyhdi01.png}{options: width="100\%" alt="Figure: geommyhdi01.png"}
#' }
#'
#' \if{latex}{
#' A sample of the output from \code{geom_myhdi()}:
#'
#' \figure{geommyhdi01.png}{options: width=10cm}
#' }
#'
#' @section Aesthetics:
#' \code{geom_myhdi} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \code{alpha}
#'   \item \code{color}
#'   \item \code{linetype}
#'   \item \code{size}
#' }
#'
#' @seealso
#'  \code{\link[ggplot2]{geom_line}}: Connect observations (x order);
#'  \code{\link[ggplot2]{geom_path}}: Connect observations;
#'  \code{\link[ggplot2]{geom_polygon}}: Filled paths (polygons);
#'  \code{\link[ggplot2]{geom_segment}}: Line segments;
#'  \code{\link[graphics]{myhdi}};
#'  \code{\link[grid]{grid.myhdi}}
#'
#' @details
#' An X-spline is a line drawn relative to control points. For each control
#' point, the line may pass through (interpolate) the control point or it may
#' only approach (approximate) the control point; the behaviour is determined
#' by a shape parameter for each control point.
#'
#' If the shape parameter is greater than zero, the spline approximates the
#' control points (and is very similar to a cubic B-spline when the shape is
#' 1). If the shape parameter is less than zero, the spline interpolates the
#' control points (and is very similar to a Catmull-Rom spline when the shape
#' is -1). If the shape parameter is 0, the spline forms a sharp corner at that
#' control point.
#'
#' For open X-splines, the start and end control points must have a shape of
#' 0 (and non-zero values are silently converted to zero).
#'
#' For open X-splines, by default the start and end control points are
#' replicated before the curve is drawn. A curve is drawn between (interpolating
#' or approximating) the second and third of each set of four control points,
#' so this default behaviour ensures that the resulting curve starts at the
#' first control point you have specified and ends at the last control point.
#' The default behaviour can be turned off via the repEnds argument.
#'
#' @inheritParams ggplot2::geom_line
#' @param geom,stat Use to override the default connection between
#'   \code{geom_myhdi} and \code{stat_myhdi}.
#' @param spline_shape A numeric vector of values between -1 and 1, which
#'        control the shape of the spline relative to the control points.
#' @param open A logical value indicating whether the spline is an open or a
#'        closed shape.
#' @param rep_ends For open X-splines, a logical value indicating whether the
#'        first and last control points should be replicated for drawing the
#'        curve. Ignored for closed X-splines.
#' @references Blanc, C. and Schlick, C. (1995), "X-splines : A Spline Model
#'             Designed for the End User", in \emph{Proceedings of SIGGRAPH 95},
#'             pp. 377-386. \url{http://dept-info.labri.fr/~schlick/DOC/sig1.html}
#' @export
#' @family myhdi implementations
#' @examples
#' set.seed(1492)
#' dat <- data.frame(x=c(1:10, 1:10, 1:10),
#'                   y=c(sample(15:30, 10), 2*sample(15:30, 10),
#'                       3*sample(15:30, 10)),
#'                   group=factor(c(rep(1, 10), rep(2, 10), rep(3, 10)))
#' )
#'
#' ggplot(dat, aes(x, y, group=group, color=group)) +
#'   geom_point() +
#'   geom_line()
#'
#' ggplot(dat, aes(x, y, group=group, color=factor(group))) +
#'   geom_point() +
#'   geom_line() +
#'   geom_smooth(se=FALSE, linetype="dashed", size=0.5)
#'
#' ggplot(dat, aes(x, y, group=group, color=factor(group))) +
#'   geom_point(color="black") +
#'   geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
#'   geom_myhdi(size=0.5)
#'
#' ggplot(dat, aes(x, y, group=group, color=factor(group))) +
#'   geom_point(color="black") +
#'   geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
#'   geom_myhdi(spline_shape=-0.4, size=0.5)
#'
#' ggplot(dat, aes(x, y, group=group, color=factor(group))) +
#'   geom_point(color="black") +
#'   geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
#'   geom_myhdi(spline_shape=0.4, size=0.5)
#'
#' ggplot(dat, aes(x, y, group=group, color=factor(group))) +
#'   geom_point(color="black") +
#'   geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
#'   geom_myhdi(spline_shape=1, size=0.5)
#'
#' ggplot(dat, aes(x, y, group=group, color=factor(group))) +
#'   geom_point(color="black") +
#'   geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
#'   geom_myhdi(spline_shape=0, size=0.5)
#'
#' ggplot(dat, aes(x, y, group=group, color=factor(group))) +
#'   geom_point(color="black") +
#'   geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
#'   geom_myhdi(spline_shape=-1, size=0.5)
geom_myhdi <- function(mapping = NULL, data = NULL, stat = "myhdi",
                         position = "identity", na.rm = TRUE, show.legend = NA,
                         inherit.aes = TRUE,
                         spline_shape=-0.25, open=TRUE, rep_ends=TRUE, ...) {
  layer(
    geom = GeomMyHdi,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(spline_shape=spline_shape,
                  open=open,
                  na.rm = na.rm,
                  rep_ends=rep_ends,
                  ...)
  )
}

#' Geom Proto
#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @export
GeomMyHdi <- ggproto("GeomMyHdi", GeomSegment,
                       required_aes = c("x"),
                       default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA)
)

#' @export
#' @rdname geom_myhdi
#' @section Computed variables:
#' \itemize{
#'   \item{x}
#'   \item{y}
#' }
stat_myhdi <- function(mapping = NULL, data = NULL, geom = "segment",
                         position = "identity", na.rm = TRUE, show.legend = NA, inherit.aes = TRUE,
                         spline_shape=-0.25, open=TRUE, rep_ends=TRUE, ...) {
  layer(
    stat = StatMyHdi,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(spline_shape=spline_shape,
                  open=open,
                  na.rm = na.rm,
                  rep_ends=rep_ends,
                  ...
    )
  )
}

#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatMyHdi <- ggproto("StatMyHdi", Stat,
                       
                       required_aes = c("x"),
                       
                       compute_group = function(self, data, scales, params,
                                                spline_shape=-0.25, open=TRUE, rep_ends=TRUE) {
                         require(hBayesDM)
                         # tf <- tempfile(fileext=".png")
                         # png(tf)
                         # plot.new()
                         # tmp <- hdi(data$x, data$y, spline_shape, open, rep_ends, draw=FALSE, NA, NA)
                         # invisible(dev.off())
                         # unlink(tf)
                         print(data$x)
                         hdi.vals<-HDIofMCMC(data$x)
                         
                         data.frame(x=hdi.vals[1], xend=hdi.vals[2],y=0,yend=0)
                       }
)