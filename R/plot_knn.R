#    vlrr: Vectorised linear regression with regularisation
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
#' @examples
#'
#' plot(X, centroids, idx)
#'
#' @export



plot_knn <- function( X, centroids, idx = NULL) {

  # check whether idx is supplied, if not then just print the data and
  # centroids, if it is supplied, then plot the groups with different colours.

  if (!is.null(idx)) {

    plot(rbind(X, centroids),type = "n")

    for (i in unique(idx)) {

      points(X[idx == i,], col = i)

    }

    k = unique(idx)

    points(centroids, col = 1, lwd = 2, pch = paste(1:k))

  } else {

    plot(rbind(X, centroids), type = "n")
    points(X, col = 1)
    points(centroids, col = 2, lwd = 2, pch = 2)

  }

}
