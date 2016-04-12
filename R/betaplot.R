library(dplyr)
library(tidyr)
library(ggplot2)

#' Illustrated interaction plot 1
#'
#' plots a diagram for a linear model with one dichtotomous group and a metric
#' predictor
#'
#' @param fixef vector of four coefficients b_0 - b_3, see Details
#' @param range a vector of two values c(xmin, xmax)
#' @param groups a vector of two strings, like c("A", "B")
#'
#' This function plots a standard interaction diagram for a model with one covariate and two groups. This model has the following parameters:
#'
#' b_0 is the intercept, the expected value of the reference group, i.e. the first element in argument groups ("A").
#' b_1 is the grand difference towards the second group (b_2 = 0)
#' b_2 is the slope of reference group
#' b_3 is the interaction effect, which is the  difference in slope towards the reference group
#'
#' The line plot is augmented with illustrations, showing the two group effects b_0 and b_1 as vertical bars and the slope parameters as triangles.
#'
#' @examples
#'
#' iiaplot_1(fixef(b_0 = -2, b_1 = 1, b_2 = 1.5, b_3 = 1))
#'
#'
#' @author Martin Schmettow
#' @import ggplot2
#' @importFrom dplyr data_frame
#' @export
#'


iiaplot_1 <-
  function(fixef = c(b_0 = 0, b_1 = -8, b_2 = 1, b_3 = 2),
           range = c(0,10),
           groups = c("A", "B"))
  {
    xmin = range[1]
    xmax = range[2]
    xcross = 0.9 * xmax

    lin_fnc <-
      function(x, grp)
        fixef[1] + fixef[2] * (grp == groups[2]) +
      fixef[3] * x +
      fixef[4] * x * (grp == groups[2])

    Lines <-
      expand.grid(x = range, group = groups) %>%
      mutate(y = lin_fnc(x, group))

    poly.A <-
      ggplot2::geom_polygon(data =
                              data_frame(
                                x = c(xcross, xmax, xmax),
                                group = groups[1],
                                y = c(lin_fnc(xcross, group),
                                      lin_fnc(xcross, group),
                                      lin_fnc(xmax,group))),
                            aes(x = x, y = y, fill = group))

    poly.B <-
      ggplot2::geom_polygon(data =
                              data_frame(
                                x = c(xcross, xmax, xmax),
                                group = groups[2],
                                y = c(lin_fnc(xcross, groups[2]),
                                      lin_fnc(xmax,groups[1]) +
                                        lin_fnc(xcross, groups[2]) -
                                        lin_fnc(xcross, groups[1]),
                                      lin_fnc(xmax,group))),
                            aes(x = x, y = y, fill = group))

    poly.Adiff <-
      ggplot2::geom_polygon(data =
                     data_frame(
                       x = c(xcross,
                             xmax,
                             xmax),
                       group = groups[1],
                       y = c(lin_fnc(xcross, groups[2]),
                             lin_fnc(xcross, groups[2]),
                             lin_fnc(xmax,groups[1]) +
                               lin_fnc(xcross, groups[2]) -
                               lin_fnc(xcross, groups[1]))),
                   aes(x = x, y = y),
                   fill = NA)


    G <-
      Lines %>%
      ggplot(aes(x = x, y = y, color = group)) +
      #geom_point() +
      geom_line() +
      poly.A +
      poly.B +
      poly.Adiff +
      geom_linerange(aes(x = 0,
                         ymin = 0, ymax = fixef[1],
                         col = groups[1])) +
      geom_linerange(aes(x = xmin - 0.02 * xmax,
                         ymin = fixef[1], ymax = fixef[1] + fixef[2],
                         col = groups[2])) +
      xlim(xmin - 0.05 * xmax, xmax)


    G

  }

