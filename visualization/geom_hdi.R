#' Draw a 95% Highest Density Interval using hBayesDM HDIofMCMC
#'
#' Draws a 95% interval. Uses \code{geom_segment} to plot.
#' Adapted code from https://github.com/hrbrmstr/ggplot2/blob/master/vignettes/extending-ggplot2.Rmd.
#' Requires the hBayesDM package.
#'
#'
#' @section Aesthetics:
#' \code{geom_hdi} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
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
#'  \code{\link[graphics]{hdi}};
#'  \code{\link[grid]{grid.hdi}}
#'
#' @details
#' Bayesian analysis often involves calculating a posterior distribution of values for some parameter.
#' To get a sense of credible values taht can then be accepted, it is useful to calculate a 95% Highest Density Interval.
#' This plots the narrowest consecutive interval in which 95% (or another specified credible range) of the posterior distribution has been found.
#'
#' @inheritParams ggplot2::geom_segment
#' @param geom,stat Use to override the default connection between
#'   \code{geom_hdi} and \code{stat_hdi}.
#' @param credible_mass a numeric value which should be between 0 and 1, passed to hBayesDM::HDIofMCMC, which
#'        indicates which credible mass to define the interval within.
#' @export
#' @family hdi implementations
#' @examples
#' set.seed(1492)
#' dat<-data.frame("x"=c(rnorm(1000,0,1),rnorm(1000,1,1)),
#'                 "people"=c(rep("Blergs",1000),rep("Spurgles",1000)))
#' 
#' ggplot(dat, aes(x=x, group=people, fill=factor(people))) +
#'   geom_histogram()+
#'   geom_hdi(aes(color=people),lineend="round",size=2,credible_mass=0.95,alpha=0.8)
#' 
#' ggplot(dat, aes(x=x, group=people, fill=factor(people))) +
#'   geom_histogram()+
#'   geom_hdi(color="black",lineend="round",size=2,credible_mass=0.99)+
#'   geom_hdi(color="white",size=1,credible_mass=0.95)
geom_hdi <- function(mapping = NULL, data = NULL, stat = "hdi",
                         position = "identity", na.rm = TRUE, show.legend = NA,
                         inherit.aes = TRUE,
                     credible_mass=0.95, ...) {
  layer(
    geom = GeomHdi,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(credible_mass=credible_mass,
                  ...)
  )
}

#' Geom Proto
#' @rdname ggproto
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @export
GeomHdi <- ggproto("GeomHdi", GeomSegment,
                       required_aes = c("x"),
                       default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA)
)

#' @export
#' @rdname geom_hdi
#' @section Computed variables:
#' \itemize{
#'   \item{x}
#' }
stat_hdi <- function(mapping = NULL, data = NULL, geom = "segment",
                         position = "identity", na.rm = TRUE, show.legend = NA, inherit.aes = TRUE,
                     credible_mass=0.95, ...) {
  layer(
    stat = StatHdi,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(credible_mass=credible_mass,
                  ...
    )
  )
}

#' @rdname ggproto
#' @format NULL
#' @usage NULL
#' @export
StatHdi <- ggproto("StatHdi", Stat,
                       
                       required_aes = c("x"),
                       
                       compute_group = function(self, data, scales, params,
                                                credible_mass=0.95) {
                         require(hBayesDM)
                         hdi.data<-HDIofMCMC(data$x,credible_mass)
                         data.frame(x=hdi.data[1],xend=hdi.data[2],y=0,yend=0)
                       }
)


#these two written with help from:
#https://plotly-book.cpsievert.me/translating-custom-ggplot2-geoms.html#fig:xspline
#and
#https://github.com/hrbrmstr/ggalt/search?utf8=%E2%9C%93&q=prefix_class&type=

to_basic.GeomHdi<-function (data, prestats_data, layout, params, p, ...) 
{
  require(tidyr)
  print(data)
  #put a confidence interval into the hovertext
  data$hovertext<-apply(data,1,
                        function(r){
                          sub(paste0(params$hoverTextAes,": -?\\d+(.\\d+)?(e\\d+)?"),
                              paste0(params$hoverTextAes,": [",signif(as.numeric(r[["x"]]),4),", ",
                                     signif(as.numeric(r[["xend"]],4)),"]"),r[["hovertext"]])
                        })
                        
  d<-tidyr::gather(data,xposinline,x,x:xend)
  d$y<-rep(1:dim(d)[1]/2,each=2)-1
  dline<-d

  #
  prefix_class(dline, "GeomPath")
}

prefix_class <- function(x, y) {
  structure(x, class = unique(c(y, class(x))))
}