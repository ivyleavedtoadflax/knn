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
#' @title Calculate group
#'
#' @description \code{find_group} calculate nearest centroid
#'
#' @param X Matrix of \code{ncol >=2}.
#' @param centroids Vector of initial centroid locations. Defaults to \code{NULL}.
#' @param k Number of randomly initiliased centroids to create. Deafults to 3, and is required in \code{centroids = NULL}.
#'
#' @examples
#'
#' X <- mtcars[, c("disp", "mpg")]
#' idx <- find_group(X, k = 2)
#'
#' @export


find_group <- function(X, centroids = NULL, k = NULL, ...) {
  X <- as.matrix(X)
  stopifnot(is.matrix(X))

  # Is centroids supplied? If not select a random sample of training examples,
  # up to length k

  if (is.null(centroids)) {
    if (is.numeric(k)) {
      centroids = X[sample(nrow(X),k),]

    } else {
      stop("If centroids are not specified, then k must be numeric.")

    }

  } else {
    # If centroids are supplied, then k = number of centroids supplied

    k = nrow(centroids)

  }

  distance_matrix <- matrix(nrow = nrow(X), ncol = k)

  # Loop through centroids calculating the norm

  for (i in 1:k) {

#     centroid_matrix <- matrix(
#       rep(centroids[i,], nrow(X)),
#       ncol = ncol(centroids),
#       byrow = TRUE
#       )

    centroids_m <- centroid_matrix(centroids[i,], nrow(X))

    error <- X - centroids_m
    error <- rowNorms(error)
    distance_matrix[,i] <- error ^ 2

  }

  # Get smallest distance, and return

  closest <- apply(distance_matrix, 1, which.min)

  return(closest)
}
