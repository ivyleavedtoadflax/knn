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
#' @title rowNorms
#'
#' @description \code{rowNorms} rowNorms
#'
#' @param x \code{x} A matrix object of \code{nrows >= 1} and \code{ncol >= 2}.
#'
#' @examples
#'
#' # a <- matrix(1:20, ncol = 2)
#'
#' #rowNorms(a)
#'

rowNorms <- function(x) {
  norms <- sqrt(rowSums(x ^ 2))

  return(norms)

}
