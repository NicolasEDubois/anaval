context("linear_link")

test_that("linear_link gives the correct y values", {
    expect_equal(linear_link(intercept = 0, slope = 1, x = c(1:3)), c(1:3))
    expect_equal(linear_link(intercept = 1, slope = 1, x = c(1:3)), c(2:4))
    expect_equal(linear_link(intercept = 0, slope = 2, x = c(1:3)), c(2, 4, 6))
    expect_equal(linear_link(intercept = 0, slope = 2, x = 10), 20)
})
test_that("linear_link_inv gives the correct x values", {
    expect_equal(linear_link_inv(intercept = 0, slope = 1, y = c(1:3)), c(1:3))
    expect_equal(linear_link_inv(intercept = 1, slope = 1, y = c(2:4)), c(1:3))
    expect_equal(linear_link_inv(intercept = 0, slope = 2, y = c(2, 4, 6)), c(1:3))
    expect_equal(linear_link_inv(intercept = 1, slope = 0, y = 10), Inf)
})
