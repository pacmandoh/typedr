# fmt: skip file
test_that("warn_once warns, tips, and suppresses repeated ids", {
  .typedr_state$warned_once$unit_warn_once <- NULL
  .typedr_state$warned_once$unit_tip_once <- NULL

  expect_warning(
    .warn_once("unit_warn_once", c("!" = "careful"), type = "warn"),
    "careful"
  )
  expect_silent(.warn_once("unit_warn_once", c("!" = "careful"), type = "warn"))

  out <- capture_typedr_cli(.warn_once(
    "unit_tip_once",
    c("i" = "tip"),
    type = "tips"
  ))
  expect_match(out, "tip", fixed = TRUE)
  expect_equal(
    capture_typedr_cli(.warn_once(
      "unit_tip_once",
      c("i" = "tip"),
      type = "tips"
    )),
    ""
  )
})

test_that("type printer registry resolves keys and rejects invalid types", {
  type_printer("EmailByName", function(value, max_items = 20) {
    sprintf("{.field value}: {.cls EmailByName} %s", value)
  })

  x <- structure(
    "named@example.com",
    typedr_name = "x_named_email",
    typedr_assertion = "EmailByName",
    typedr_const = FALSE,
    class = c("typedr_value", "character")
  )

  out <- capture_typedr_cli(print(x))
  expect_match(out, "named@example.com", fixed = TRUE)
  expect_null(.typedr_get_type_printer(NULL))
  expect_error(
    .typedr_type_printer_key(list("bad")),
    class = "typedr_type_printer_error"
  )
})
