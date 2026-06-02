test_that("#47", {
  f <- typedr::Integer()?function() {
    typedr::Double()?foo <- local({
      return(as.double(1))
    })
    as.integer(1)
  }

  expect_no_error(f())
})
