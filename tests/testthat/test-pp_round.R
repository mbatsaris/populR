# load data
data('src', package = "populR")
data('trg', package = "populR")

# estimate population counts using awi and vwi
awi <- suppressWarnings(pp_estimate(trg, src, sid = sid, spop = pop, method = awi))
vwi <- suppressWarnings(pp_estimate(trg, src, sid = sid, spop = pop, volume = floors, method = vwi))
awi$pp_est_text <- as.character(awi$pp_est)

test_that("argument errors", {
  # misspelled object
  expect_error(
    pp_round(x = awis, tpop = pp_est, spop = pop, sid = sid),
    "object 'awis' not found"
  )

  # misspelled object
  expect_error(
    pp_round(tpop = pp_est, spop = pop, sid = sid),
    "`x` is absent"
  )

  # missplelled sid
  expect_error(
    pp_round(x = awi, tpop = pp_est, spop = pop, sid = sids),
    "sids cannot be found"
  )
})

test_that("non numeric fields", {
  expect_error(
    pp_round(x = awi, tpop = pp_est_text, spop = pop, sid = sid),
    "pp_est_text must be numeric"
  )
})
