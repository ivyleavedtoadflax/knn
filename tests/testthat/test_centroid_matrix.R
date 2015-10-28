

test_that("centroid_matrix is working as expected",
          {

            # Get centroid matrix.

            foo <- centroid_matrix(c(1,4), 10)

            bar <- cbind(
              rep(1, 10),
              rep(4, 10)
              )

            expect_identical(foo, bar)

            foo <- centroid_matrix(c(1,4), 10)

            # Try on longer examples

            x <- c(1,4,10,12,18)

            foo <- centroid_matrix(x, 10)

            bar <- matrix(rep(x, 10), ncol = length(x), byrow = TRUE)

            expect_identical(foo, bar)

            # More general tests.

            expect_is(foo, "matrix")

          })
