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
#' # knn(X)
#'
#' @export
#'
#'


knn <- function(X, centroids = NULL, k = 3, imax = NULL, ...) {

  ## Run one iteration as standard

  idx <- find_group(X, k = k)
  centroids <- centroid_mean(X, idx)

  idx2 <- find_group(X, centroids, k = k)
  centroids2 <- centroid_mean(X, idx2)

  i <- 1

  ## Setup a df for storing the outputs

  out_mat <- cbind(
    rbind(centroids, centroids2),
    c(
      rep(0, nrow(centroids)),
      rep(i, nrow(centroids))
    )
  )

  ## Set a threshold for 'convergence'. Some thought needs to go into how best to
  ## do this.

  while (sum(centroids - centroids2) > 0.000005 ) {

    ## If a number of iterations was specified, break the function when max
    ## iterations has been reached

    if (!is.null(imax)) {
      if(i == imax) break
    }

    idx <- find_group(X, centroids2, k = k)
    centroids <- centroid_mean(X, idx)

    idx2 <- find_group(X, centroids, k = k)
    centroids2 <- centroid_mean(X, idx2)

    ## Add to accumulator

    i <- i + 1

    ## Output results for plot function

    out_mat <- rbind(
      out_mat,
      cbind(
        centroids2,
        i
      )
    )

  }

  out <- list(
    groups = idx2,
    centroids = centroids2,
    steps = i,
    centroid_path = out_mat
  )

  return(out)

}
