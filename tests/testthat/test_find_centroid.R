
test_that(
  "find_centroid and centroid_mean are working as expected.",
  {

    # Create some test data based on three centroids.

    a <- rbind(
      cbind(
        rnorm( 30, mean = 10, sd = 2),
        rnorm( 30, mean = 10, sd = 2)
      ),
      cbind(
        rnorm( 30, mean = 20, sd = 2),
        rnorm( 30, mean = 20, sd=  2)
      ),
      cbind(
        rnorm( 30, mean = 30, sd = 2),
        rnorm( 30, mean = 30, sd = 2)
      )
    )

    # Set centroids.

    centroids <- matrix(rep(seq(10,30,10),each = 2), ncol = 2, byrow = TRUE)

    idx <- find_centroid(a, centroids)

    # Check that the find_centroid() makes decent clusters.

    expect_equal( idx[1:30], rep(1, 30))
    expect_equal( idx[31:60], rep(2, 30))
    expect_equal( idx[61:90], rep(3, 30))

    # More general tests.

    expect_equal(length(idx), nrow(a))
    expect_is(idx, "integer")

    # Now test centroid_mean against clusters to see whether it finds the same
    # clusters as the original. Use a tolerance of one to allow for rounding
    # errors. Is this too big?

    expect_equal(centroid_mean(a, idx), centroids, tolerance = 1)

  }
  )
