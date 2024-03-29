# load data
data('src', package = "populR")
data('trg', package = "populR")

test_that("argument errors", {
  # test on missing method
  expect_error(
    suppressWarnings(pp_estimate(target = trg, source = src, sid = sid, spop = pop)),
    "method is required"
  )

  # test on missing spop
  expect_error(
    suppressWarnings(pp_estimate(target = trg, source = src, sid = sid, method = awi)),
    "spop is required"
  )

  # test on missing target
  expect_error(
    suppressWarnings(pp_estimate(source = src, sid = sid, method = awi)),
    "target is required"
  )

  # test on missing source
  expect_error(
    suppressWarnings(pp_estimate(target = trg, sid = sid, spop = pop, method = awi)),
    "source is required"
  )

  # misspelled target object
  expect_error(
    suppressWarnings(pp_estimate(target = trgs, source = src, sid = sid, spop = pop, volume = floors,
                method = vwi)), "object 'trgs' not found"
  )

  # misspelled source object
  expect_error(
    suppressWarnings(pp_estimate(target = trg, source = srcs, sid = sid, spop = pop, volume = floors, method = vwi)),
    "object 'srcs' not found"
  )
})


test_that("check on results", {
  # check areal weighting interpolation (awi) results
  awi_res <- suppressWarnings(pp_estimate(target = trg, source = src, sid = sid, spop = pop, method = awi))
  expect_equal(
    sum(awi_res$pp_est),
    sum(src$pop)
  )
  # check volume weighting interpolation (vwi) results
  vwi_res <- suppressWarnings(pp_estimate(target = trg, source = src, sid = sid, spop = pop, volume = floors,
                         method = vwi))
  expect_equal(
    sum(vwi_res$pp_est),
    sum(src$pop)
  )
})

test_that("non numeric fields", {
  src$pop_text <- as.character(src$pop)
  expect_error(
    suppressWarnings(pp_estimate(target = trg, source = src, sid = sid, spop = pop_text, method = awi)),
    "pop_text must be numeric"

  )
})




