resampling = function(lrn, instance) {

  # inner resampling
  r.inner = resample(learner = lrn, task = instance$task.inner, resampling = instance$cv.inner,
    measures = list(mmce, timetrain), show.info = FALSE)

  # outer evaluation
  r.outer = resample(learner = lrn, task = instance$task.outer, resampling = instance$eval,
    measures = list(mmce, timetrain), extract = getFilteredFeatures,
    show.info = FALSE)

  ret = list(
    measures.test.inner = r.inner$measures.test,
    measures.test.outer = r.outer$measures.test,
    features.outer = r.outer$extract[[1]]
  )

  return(ret)
}
