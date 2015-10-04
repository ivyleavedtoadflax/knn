
test_that(
  "rowNorms returns euclidean norm",
  {
    a <- matrix(1:20,ncol=2)
    foo <- rowNorms(a)

    expect_equal(foo[1], norm(as.matrix(a[1, ]), type = "f"))
    expect_equal(foo[5], norm(as.matrix(a[5, ]), type = "f"))
    expect_equal(foo[10], norm(as.matrix(a[10, ]), type = "f"))

    # And just out of curiousity:

    expect_equal(foo[1], sqrt(sum(a[1, ] ^ 2)))
    expect_equal(foo[5], sqrt(sum(a[5, ] ^ 2)))
    expect_equal(foo[10], sqrt(sum(a[10, ] ^ 2)))

  }
  )
