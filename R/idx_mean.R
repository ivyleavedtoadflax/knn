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
#' @title Calculate centroid mean
#'
#' @description \code{idx_mean} calculate centroid mean
#'
#' @examples
#'
#' idx_mean(X, idx)
#'
#' @export



idx_mean <- function(X, idx) {

  k = unique(idx)

  out_matrix <- matrix(nrow = k, ncol = X)

  for (i in 1:k) {

    out_matrix[i,] <- colMeans(X[i == idx,])

  }
  return(out_matrix)
}
