makeLrn = function(pars) {
  lrn = makeLearner(pars$selected.learner)

  # remove parameters with NA value
  nas = unlist(lapply(pars, is.na))
  pars = pars[!nas]

  hyperpars = pars[!names(pars) %in% c("selected.learner", "fw.perc")]

  names.hp = sapply(names(hyperpars), function(n) {
    ns = strsplit(n, ".", fixed = TRUE)[[1]]
    ns[length(ns)]
  })
  names(hyperpars) = names.hp


  if (pars$selected.learner == "classif.glmnet") {
    # for s = 0 include 0 in lambda sequence
    if ("s" %in% names(pars) && pars$s == 0) {
      lambda = c(exp(-(2:100)), 0)
      pars = c(pars, list(lambda = lambda))
    }
  }

  lrn = setHyperPars(lrn, par.vals = hyperpars)

  return(lrn)
}

