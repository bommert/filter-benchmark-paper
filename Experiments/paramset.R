mm = makeModelMultiplexer(list("classif.glmnet", "classif.kknn", "classif.svm.hd"))

ps = makeModelMultiplexerParamSet(mm,
  classif.glmnet = makeParamSet(
    makeDiscreteParam("alpha", values = 0L),
    makeNumericParam("s", lower = -15, upper = 15, trafo = function(x) 2^x)
  ),
  classif.kknn = makeParamSet(
    makeDiscreteParam("kernel", values = "rectangular"),
    makeIntegerParam("k", lower = 1L, upper = 20L)
  ),
  classif.svm.hd = makeParamSet(
    makeNumericParam("gamma", lower = -15, upper = 15, trafo = function(x) 2^x),
    makeNumericParam("cost", lower = -15, upper = 15, trafo = function(x) 2^x)
  )
)

ps = c(ps, makeParamSet(
  makeNumericParam("fw.perc", lower = 0, upper = 1)
))
