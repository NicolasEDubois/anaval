context("simulation")

test_that("norm_lin_dataset_sim output is a matrix", {
    data(ref_calib_doe)
    expect_is(norm_lin_dataset_sim(ref_calib_doe, intercept = 1, slope = 1, SDrun = 1.5,
        SDrep = 3, biais = 0), "matrix")
})

test_that("norm_lin_dataset_sim total variability is correct", {
    # The total variability is SDrun^2 + SDrep^2 (in the 2 first tests 1.5^2 + 3^2 = 11.25) An deviation
    # of 2.5% is accepted between the total variability calculated from the simulated values and the
    # theoritical total variability.

    ref_sim1 <- norm_lin_dataset_sim(ref_sim_tot_var, intercept = 1, slope = 1, SDrun = 1.5, SDrep = 3, biais = 0)[, "Response"]
    ref_sim1_var <- var(linear_link_inv(intercept = 1, slope = 1, as.numeric(ref_sim1)))
    expect_that(abs(ref_sim1_var - 11.25)/11.25 * 100 < 2.5, is_true())
    rm(ref_sim1,ref_sim1_var)

    ref_sim2 <- norm_lin_dataset_sim(ref_sim_tot_var, intercept = 1, slope = 1, SDrun = 10, SDrep = 10, biais = 0)[, "Response"]
    ref_sim2_var <- var(linear_link_inv(intercept = 1, slope = 1, as.numeric(ref_sim2)))
    expect_that(abs(ref_sim2_var - 200)/200 * 100 < 2.5, is_true())
    rm(ref_sim2,ref_sim2_var)

    ref_sim3 <- norm_lin_dataset_sim(ref_sim_tot_var, intercept = -10, slope = -25, SDrun = 10, SDrep = 10, biais = 0)[, "Response"]
    ref_sim3_var <- var(linear_link_inv(intercept = -10, slope = -25, as.numeric(ref_sim3)))
    expect_that(abs(ref_sim3_var - 200)/200 * 100 < 2.5, is_true())
    rm(ref_sim3,ref_sim3_var)
})

# A faire test du biais !!!!!!!!!!!!!!!!!!!!!!!!
