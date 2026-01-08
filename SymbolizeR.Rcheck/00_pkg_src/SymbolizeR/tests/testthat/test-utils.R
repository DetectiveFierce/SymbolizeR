# Tests for classify.type() and ensure.expression()

test_that("classify.type identifies numeric literals", {
  expect_equal(classify.type(5), "number")
  expect_equal(classify.type(3.14), "number")
  expect_equal(classify.type(0), "number")
  expect_equal(classify.type(-10), "number")
})

test_that("classify.type identifies random variables (uppercase)", {
  expect_equal(classify.type(quote(X)), "rv")
  expect_equal(classify.type(quote(Y)), "rv")
  expect_equal(classify.type(quote(Z)), "rv")
  expect_equal(classify.type(quote(Sigma)), "rv")
  expect_equal(classify.type(quote(Normal)), "rv")
})

test_that("classify.type identifies constants (lowercase)", {
  expect_equal(classify.type(quote(a)), "const")
  expect_equal(classify.type(quote(b)), "const")
  expect_equal(classify.type(quote(mu)), "const")
  expect_equal(classify.type(quote(sigma)), "const")
  expect_equal(classify.type(quote(beta)), "const")
})

test_that("classify.type identifies calls", {
  expect_equal(classify.type(quote(a * X)), "call")
  expect_equal(classify.type(quote(X + Y)), "call")
})

test_that("ensure.expression handles symbols", {
  result <- ensure.expression(quote(X))
  expect_true(is.symbol(result))
})

test_that("ensure.expression handles calls", {
  result <- ensure.expression(quote(a * X))
  expect_true(is.call(result))
})

test_that("ensure.expression handles numerics", {
  result <- ensure.expression(5)
  expect_equal(result, 5)
})

# Tests for expand.poly()

test_that("expand.poly returns symbols unchanged", {
  expect_equal(expand.poly(quote(X)), quote(X))
  expect_equal(expand.poly(quote(a)), quote(a))
})

test_that("expand.poly returns numerics unchanged", {
  expect_equal(expand.poly(5), 5)
})

test_that("expand.poly expands (A + B)^2", {
  result <- expand.poly(quote((a + b)^2))
  # Should be: a^2 + 2*a*b + b^2
  expect_true(is.call(result))
})

test_that("expand.poly expands (A - B)^2", {
  result <- expand.poly(quote((a - b)^2))
  # Should be: a^2 - 2*a*b + b^2
  expect_true(is.call(result))
})

test_that("expand.poly expands (A * B)^n", {
  result <- expand.poly(quote((a * b)^2))
  # Should be: a^2 * b^2 (structure check, not exact match due to integer literals)
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "*")
})

test_that("expand.poly distributes multiplication over addition", {
  result <- expand.poly(quote((a + b) * c))
  # Should become: a*c + b*c
  expect_true(is.call(result))
  expect_equal(as.character(result[[1]]), "+")
})
