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
#' @title centroid_matrix
#'
#' @description \code{centroid_matrix} rowNorms
#'
#' @param x \code{x} A vector of integer or double with elements \code{n >= 1}.
#' @param m \code{m} An integer declaring the number of unlabelled training examples.
#'
#' @examples
#'
#' # X is a matrix of unlabelled training examples
#'
#' # X <- matrix(rep( 0, 10), ncol = 2)
#'
#' # y is a vector of initial centroids
#'
#' centroids <- c(1,10)
#'
#' # X - centroid_matrix(x = centroids, m = nrow(X))
#'
#'

centroid_matrix <- function(x, m) {

  y <- matrix(
    rep(x, m),
    ncol = length(x),
    byrow = TRUE
  )

  return(y)

}
