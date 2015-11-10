#    knn: Simple implementation of k-nearest-neighbours
#    A package for the R statistical environment
#    Copyright (C) 2015  Matthew Upson <ivyleavedtoadflax@gmail.com>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#' @title Plot k-nearest neighbours
#'
#' @description \code{plot_knn} plot k nearest neighbours
#'
#' @param X \code{X} Matrix of \code{ncol >=2}.
#' @param centroid_means \code{centroid_means} Centroid means.
#' @param centroids \code{centroids} Vector of initial centroid locations. Defaults to \code{NULL}.
#'
#' @examples
#'
#' # plot(X, centroid_means, centroids)
#'
#' @export

plot_knn <- function(X, centroid_means, centroids = NULL) {
  X <- as.matrix(X)
  colnames(X) <- NULL

  # check whether centroids is supplied, if not then just print the data and
  # centroid_means, if it is supplied, then plot the groups with different
  # colours.

  if (!is.null(centroids)) {
    plot(
      rbind(X, centroid_means),
      type = "n",
      xlab = expression(x[1]),
      ylab = expression(x[2])
    )

    for (i in unique(centroids)) {
      points(X[centroids == i,], col = i)

    }

    k = unique(centroids)

    points(centroid_means, col = 1, lwd = 2, pch = 2)

  } else {
    plot(
      rbind(X, centroid_means),
      type = "n",
      xlab = expression(x[1]),
      ylab = expression(x[2])
    )
    points(X, col = 1)
    points(centroid_means, col = 2, lwd = 2, pch = 2)

  }

}
