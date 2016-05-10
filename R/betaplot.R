library(dplyr)
library(tidyr)
library(ggplot2)

#' Illustrated interaction plot 4
#'
#' plots a diagram for a linear model with one dichtotomous group and a metric
#' predictor
#'
#' @param fixef list of four coefficients b_0 - b_3, see Details
#' @param range a vector of two values c(xmin, xmax)
#' @param groups a vector of two strings, like c("A", "B")
#' @param label vector of strings ("covariate", "outcome")
#'
#' This function plots a standard interaction diagram for a model with
#' one covariate and two groups.
#' This model has the following parameters:
#'
#' b_0 is the intercept, the expected value of the reference group,
#' i.e. the first element in argument groups ("A").
#' b_1 is the grand difference towards the second group (b_2 = 0)
#' b_2 is the slope of reference group
#' b_3 is the interaction effect, which is the  difference in slope towards the reference group
#'
#' The parameters can also be given as an unnamed list, as long as the elements comply to the order.
#'
#' The line plot is augmented with illustrations, showing the two group effects
#' b_0 and b_1 as vertical bars and the slope parameters as triangles.
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


iiaplot_4 <-
  function(fixef = list(b_0 = 0, b_1 = -8, b_2 = 1, b_3 = 2),
           range = c(0,10),
           groups = c("A", "B"),
           label = c("covariate", "outcome"))
  {
    xmin = range[1]
    xmax = range[2]
    xcross = 0.9 * xmax

    if(is.null(names(fixef)))  names(fixef) <- paste0("b_", 0:3)

    lin_fnc <-
      function(x, grp)  fixef[["b_0"]] + fixef[["b_1"]] * (grp == groups[2]) +
      fixef[["b_2"]] * x +
      fixef[["b_3"]] * x * (grp == groups[2])

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
      geom_linerange(aes(x = 0,
                         ymin = 0, ymax = fixef[["b_0"]],
                         col = groups[1])) +
      geom_linerange(aes(x = xmin - 0.02 * xmax,
                         ymin = fixef[["b_0"]], ymax = fixef[["b_0"]] + fixef[["b_1"]],
                         col = groups[2])) +
      poly.A +
      poly.B +
      poly.Adiff +
      xlim(xmin - 0.05 * xmax, xmax) +
      xlab(label[1]) +
      ylab(label[2])
    # if(fixef[["b_1"]] != 0) G <- G +
    #   geom_linerange(aes(x = xmin - 0.02 * xmax,
    #                      ymin = fixef[["b_0"]], ymax = fixef[["b_0"]] + fixef[fixef[["b_2"]]],
    #                      col = groups[2]))
    # if(fixef[["b_2"]] != 0) {
    #   G <- G + poly.A
    #   if(fixef[["b_1"]] != 0){
    #     G <- G + poly.B
    #     if(fixef[["b_3"]] != 0)
    #       G <- G + poly.Adiff
    #   }
    # }


    G

  }


#' Illustrated regression line
#'
#' plots a diagram for a linear model with one dichtotomous group and a metric
#' predictor
#'
#' @param fixef list of four coefficients b_0 and b_2, see Details
#' @param range a vector of two values c(xmin, xmax)
#' @param groups a vector of two strings, like c("A", "B")
#' @param label vector of strings ("covariate", "outcome")
#'
#' This function plots a standard interaction diagram for a model with
#' one covariate and two groups.
#' This model has the following parameters:
#'
#' b_0 is the intercept
#' b_2 is the slope of reference group
#'
#' The parameters can also be given as an unnamed list, as long as the elements comply to the order.
#'
#' The line plot is augmented with illustrations, showing the intercept as vertical bar and
#' slope parameters as triangles.
#'
#' @examples
#'
#' iiaplot_4(fixef(b_0 = -2, b_2 = 1.5))
#'
#'
#' @author Martin Schmettow
#' @import ggplot2
#' @importFrom dplyr data_frame
#' @export
#'


iiaplot_2 <-
  function(fixef = list(b_0 = 0.5,  b_2 = 1),
           range = c(0,10),
           label = c("covariate", "outcome"))
  {
    xmin = range[1]
    xmax = range[2]
    xcross = 0.9 * xmax

    if(is.null(names(fixef)))  names(fixef) <- paste0("b_", c(0,2))

    lin_fnc <-
      function(x)  fixef[["b_0"]] +
      fixef[["b_2"]] * x

    Lines <-
      expand.grid(x = range) %>%
      mutate(y = lin_fnc(x))

    poly.A <-
      ggplot2::geom_polygon(data =
                              data_frame(
                                x = c(xcross, xmax, xmax),
                                y = c(lin_fnc(xcross),
                                      lin_fnc(xcross),
                                      lin_fnc(xmax))),
                            aes(x = x, y = y))



    G <-
      Lines %>%
      ggplot(aes(x = x, y = y)) +
      geom_line() +
      geom_linerange(aes(x = 0,ymin = 0, ymax = fixef[["b_0"]])) +
      poly.A +
      xlim(xmin - 0.05 * xmax, xmax) +
      xlab(label[1]) +
      ylab(label[2])

    G

  }


#' Illustrated ANOVA
#'
#' plots a diagram for a linear model with one dichtotomous group and a metric
#' predictor
#'
#' @param fixef list of three coefficients b_0, b_1, b_3, see Details
#' @param groups a vector of three strings, like c("A", "B", "C)
#' @param label vector of strings ("group", "outcome")
#'
#' This function plots a diagram for an ANOVA model with
#' three groups.
#' This model has the following parameters:
#'
#' b_0 is the intercept, the expected value of the reference group,
#' i.e. the first element in argument groups ("A").
#' b_1 and B_2 are the mean differences
#'
#' The parameters can also be given as an unnamed list, as long as the elements comply to the order.
#'
#' The line plot is augmented with illustrations, showing the two group effects
#' b_0 and b_1 as vertical bars and the slope parameters as triangles.
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
  function(fixef = list(b_0 = 8, b_1 = -1, b_2 = 2),
           groups = c("A", "B", "C"),
           label = c("group", "outcome"))
  {

    if(is.null(names(fixef)))  names(fixef) <- paste0("b_", 0:2)

    Lines <-
      dplyr::data_frame(group = groups,
                              ymin = c(0, fixef$b_0, fixef$b_0),
                              ymax = c(fixef$b_0, fixef$b_0 + fixef$b_1, fixef$b_0 + fixef$b_2))

    G <-
      Lines %>%
      ggplot(aes(x = group, ymin = ymin, ymax = ymax )) +
      geom_linerange() +
      xlab(label[1]) +
      ylab(label[2])

    G
  }
