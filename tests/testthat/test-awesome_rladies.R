test_that("assess awesome_rladies", {
  # writing a single message ----
  expected_1 <- write_rladies(1)
  expected_2 <- write_rladies(2)
  expect_type(expected_1, "character")
  expect_type(expected_2, "character")
  expect_equal(expected_1, "There is 1 awesome RLady!")
  expect_equal(expected_2, "There are 2 awesome RLadies!")

  # writing multiple messages ----
  expected_multiple <- compose_rladies(1:2)
  expect_length(expected_multiple, 2)

  # catching errors ----
  expect_error(compose_rladies(0))
  expect_error(compose_rladies(-1))
  expect_error(compose_rladies(3.14))
  expect_error(compose_rladies("one"))

})
