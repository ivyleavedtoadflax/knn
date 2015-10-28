

test_that("find_centroid and centroid_mean are working as expected.",
          {
            # Create some test data based on three centroids.

            a <- rbind(cbind(rnorm(30, mean = 10, sd = 2),
                             rnorm(30, mean = 10, sd = 2)),
                       cbind(rnorm(30, mean = 20, sd = 2),
                             rnorm(30, mean = 20, sd = 2)),
                       cbind(rnorm(30, mean = 30, sd = 2),
                             rnorm(30, mean = 30, sd = 2)))

            a <- as.data.frame(a)

            # Set centroids.

            centroids <- matrix(rep(seq(10, 30, 10),each = 2),
                                ncol = 2,
                                byrow = TRUE)

            idx <- find_centroid(a, centroids)

            #plot_knn(a, centroids, idx)

            # Check that the find_centroid() makes decent clusters.

            expect_equal(idx[1:30], rep(1, 30))
            expect_equal(idx[31:60], rep(2, 30))
            expect_equal(idx[61:90], rep(3, 30))

            # More general tests.

            expect_equal(length(idx), nrow(a))
            expect_is(idx, "integer")

            # Now test centroid_mean against clusters to see whether it finds the same
            # clusters as the original. Use a tolerance of one to allow for rounding
            # errors. Is this too big?

            expect_equal(centroid_mean(a, idx), centroids, tolerance = 1)

          })



test_that("find_centroid and centroid_mean are working as expected on real data",
          {
            # Create some test data based on three centroids.

            a <- readRDS("real_data.Rds")
            #a <- readRDS("real_data.Rds")

            # Set centroids.

            centroids <- matrix(rep(seq(0, 8, length.out = 3),each = 2),
                                ncol = 2,
                                byrow = TRUE)

            idx <- find_centroid(a)#, centroids)

            #plot_knn(a, centroids, idx)

            centroids <- centroid_mean(a,idx)
            idx <- find_centroid(a,centroids)

            #plot_knn(a, centroids, idx)

            centroids <- centroid_mean(a,idx)
            idx <- find_centroid(a,centroids)

            #plot_knn(a, centroids, idx)

            centroids <- centroid_mean(a,idx)
            idx <- find_centroid(a,centroids)

            #plot_knn(a, centroids, idx)

            centroids <- centroid_mean(a,idx)
            idx <- find_centroid(a,centroids)

            ##plot_knn(a, centroids, idx)
            # More general tests.

            expect_equal(length(idx), nrow(a))
            expect_is(idx, "integer")

            # Now test centroid_mean against clusters to see whether it finds the same
            # clusters as the original. Use a tolerance of one to allow for rounding
            # errors. Is this too big?

            #expect_equal(centroid_mean(a, idx), centroids, tolerance = 1)

          })


test_that("Test situation if centroids are not specified in find_centroid call.",
          {
            X <- mtcars[, c("disp", "mpg")]

            idx <- find_centroid(X)

            # k defaults to 3

            expect_is(idx, "integer")
            expect_equal(length(idx), nrow(X))

            # Sepecify k manually

            idx1 <- find_centroid(X, k = 2)
            expect_is(idx1, "integer")
            expect_equal(length(idx1), nrow(X))

          })
