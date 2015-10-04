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
#' @title Calculate nearest centroid
#'
#' @description \code{idx} calculate nearest centroid
#'
#' @examples
#'
#' idx(X)
#'
#' @export


idx <- function(X, centroids = NULL, k = NULL) {

  # Is centroids supplied? If not select a random sample of training examples,
  # up to length k

  if (is.null(centroids)) {


    centroids = X[sample(nrow(X),k),]

  } else {

    # If centroids are supplied, then k = number of centrodis supplied

    k = nrow(centroids)

  }

  distance_matrix <- matrix(nrow = nrow(X), ncol = k)

  # Loop through centroids calculating the norm

  for (i in 1:k) {

    error <- X - centroids[i,]
    error <- sqrt(rowSums(error ^ 2))

    distance_matrix[,i] <- error

  }

  # Get smallest distance, and return

  closest <- apply(distance_matrix, 1, which.min)

  return(closest)
}
