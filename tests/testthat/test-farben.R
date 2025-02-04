

test_that("constructor works", {

  pdf(NULL)  # Redirect graphical output to a null device

  # Contains random character
  expect_error(
    farben(c("red", "blue", "ugly"))
  )

  # Contains NA
  expect_error(
    farben(c("red", "blue", NA))
  )

  # Contains numeric
  expect_error(
    farben(c("red", "blue", 1))
  )

  # Valid hex codes with different cases should work.
  f_hex <- farben(c("#683F8C", "#0F618A"))
  expect_s3_class(f_hex, "farben")

})




test_that("farben demo works",{

  expect_error(
    farben_demo(c("red", "blue")),
    "Input must be of class 'farben'"
  )

  expect_error(
    farben_demo(farben(rep("red", 51))),
    "Too many colors given"
  )


  # Check that the function runs without errors for valid input
  col <- farben(c("red", "blue", "yellow"))

  expect_silent(farben_demo(col))
  expect_silent(farben_demo(col, include = "all"))
  expect_silent(farben_demo(col, include = "line"))
  expect_silent(farben_demo(col, include = "scatter"))
  expect_silent(farben_demo(col, include = "bar"))

  # Check that an invalid include argument triggers an error
  expect_error(
    farben_demo(col, include = "invalid_option"),
    "Invalid input for `include`"
  )

  # Check that setting a background color works
  expect_silent(farben_demo(col, bg = "gray"))
  expect_silent(farben_demo(col, bg = "#F0E68C"))  # Hex code


  # Check edge cases for small and large palettes
  col_min <- farben(c("red"))  # Single color
  col_max <- farben(rep(c("red", "blue", "yellow"), length.out = 50))  # Max allowed colors

  expect_silent(farben_demo(col_min))
  expect_silent(farben_demo(col_max))


  dev.off() # Close NULL device

})
