# argument errors

    Code
      pp_estimate(target = trg, source = src, sid = sid, spop = pop)
    Condition
      Error in `pp_estimate()`:
      ! `method` is absent but must be supplied.
    Code
      pp_estimate(target = trg, source = src, sid = sid, method = awi)
    Condition
      Error in `pp_estimate()`:
      ! `spop` is absent but must be supplied.
    Code
      pp_estimate(source = src, sid = sid, method = awi)
    Condition
      Error in `pp_estimate()`:
      ! `target` is absent but must be supplied.
    Code
      pp_estimate(target = trg, sid = sid, spop = pop, method = awi)
    Condition
      Error in `pp_estimate()`:
      ! `source` is absent but must be supplied.

