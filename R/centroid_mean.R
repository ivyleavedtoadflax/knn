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
#' @title Calculate centroid mean
#'
#' @description \code{centroid_mean} calculate centroid mean
#'
#' @param X \code{X} Matrix of \code{ncol >=2}.
#' @param group \code{group} Vector of groups for which centroid means will be calculated.
#'
#' @examples
#'
#' #centroid_mean(X, idx)
#'
#' @export

centroid_mean <- function(X, group) {
  k = length(unique(group))

  out_matrix <- matrix(nrow = k, ncol = ncol(X))

  for (i in 1:k) {
    out_matrix[i,] <- colMeans(X[i == group,])

  }
  return(out_matrix)
}
