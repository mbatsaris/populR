# load data
data('src', package = "populR")
data('trg', package = "populR")

# estimate population counts using awi and vwi
awi <- suppressWarnings(pp_estimate(trg, src, sid = sid, spop = pop, method = awi))
vwi <- suppressWarnings(pp_estimate(trg, src, sid = sid, spop = pop, volume = floors, method = vwi))

test_that("argument errors", {
  # test on missing argument
  expect_snapshot(error = TRUE, {
    pp_compare(x = awi, estimated = pp_est,
               title ='awi')
    pp_compare(estimated = pp_est, actual = rf,
               title ='awi')
  })

  # test on misspelled x
  expect_error(
    pp_compare(x = awis, estimated = pp_est, actual = rf,
            title ='awi'),
    "object 'awis' not found"
  )

})

test_that("check on results", {
  expected_awi <- pp_compare(x = awi, estimated = pp_est, actual = rf,
                             title ='awi')
  expected_vwi <- pp_compare(x = vwi, estimated = pp_est, actual = rf,
                          title ='vwi')

  # check on return - length is not working as expected though
  expect_length(
    lengths(expected_awi), 4
  )

  expect_length(
    lengths(expected_vwi), 4
  )

})

test_that("non numeric fields", {
  awi$pp_est_text <- as.character(awi$pp_est)
  expect_error(
    pp_compare(x = awi, estimated = pp_est_text, actual = pp_est_text,
          title ='awi'),
    "pp_est_text must be numeric"
  )
})
