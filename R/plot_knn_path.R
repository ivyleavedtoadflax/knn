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
#' @title Plot centroids paths
#'
#' @description \code{plot_knn_path} Plot centroids paths
#'
#' @param \code{centroids_all} An input dataframe comprising three variable: \code{x}, \code{y} are coordinates of the locations of centroids, \code{i} is number of the centroid.
#'
#' @examples
#'
#' plot_knn_path(X, centroids_all)
#'
#' @export


plot_knn_path <- function(X, centroids_all) {

  plot(rbind(X,as.matrix(centroids_all[,-3])), type = "n")
  points(X)

  for (i in unique(centroids_all$i)) {

    centroid_matches <- centroids_all$i == i
    num_iter <- length(centroids_all[centroids_all$i == i,])
    points(centroids_all[centroid_matches,],type = "o", col = i, pch = 16)

  }
}
