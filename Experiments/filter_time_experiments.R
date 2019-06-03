library(batchtools)
library(OpenML)
library(dplyr)


reg = makeExperimentRegistry(file.dir = "time", packages = c("mlr", "microbenchmark"))

Bioresponse = getOMLDataSet(4134)
task = convertOMLDataSetToMlr(Bioresponse)


partPTask = function(data, job, part) {
  task.data = getTaskData(data, target.extra = TRUE)
  n.feats.new = round(getTaskNFeats(data) * part)
  ids.feats.new = sample(getTaskNFeats(data), n.feats.new)
  data.new = cbind(task.data$data[, ids.feats.new], target = task.data$target)
  new.task = makeClassifTask(data = data.new, target = "target")
  return(new.task)
}

partNTask = function(data, job, part) {
  task.data = getTaskData(data, target.extra = TRUE)

  # stratified selection of observations
  rdesc = makeResampleDesc("Holdout", split = part, stratify = TRUE)
  rin = makeResampleInstance(rdesc, data)
  ids.obs.new = rin$train.inds[[1]]

  data.new = cbind(task.data$data[ids.obs.new, ], target = task.data$target[ids.obs.new])
  new.task = makeClassifTask(data = data.new, target = "target")
  return(new.task)
}


addProblem(name = "partP", seed = 1,
  data = task,
  fun = partPTask
)

addProblem(name = "partN", seed = 2,
  data = task,
  fun = partNTask
)

addProblem(name = "all", seed = 3,
  data = task
)



addAlgorithm(
  name = "filter.all",
  fun = function(job, data, instance, fw.method) {
    source("config.R")
    mb = microbenchmark(times = 10,
      generateFilterValuesData(task = instance, method = fw.method))
    return(mb$time)
  }
)


addAlgorithm(
  name = "filter.part",
  fun = function(job, data, instance, fw.method, fw.perc) {
    source("config.R")
    nselect = round(fw.perc * getTaskNFeats(data))
    mb = microbenchmark(times = 10,
      generateFilterValuesData(task = data, method = fw.method, nselect = nselect))
    return(mb$time)
  }
)



# filters that we want to use
lf = listFilterMethods(tasks = TRUE, features = TRUE)
lf = lf[lf$task.classif & lf$feature.numerics, ]
filters.all = as.character(lf$id)

# exclude filters that are too slow or have a faster implementation
exclude = c("glmnet.lasso", "permutation.importance", "relief",
  "randomForestSRC.var.select", "randomForestSRC.rfsrc", "randomForest.importance",
  "gain.ratio", "information.gain", "symmetrical.uncertainty")
filters = setdiff(filters.all, exclude)
filters = c(filters, "limma", "sam")


design.filter.all = expand.grid(
  fw.method = filters,
  stringsAsFactors = FALSE
)

design.filter.part = expand.grid(
  fw.method = filters,
  fw.perc = 1:10 / 10,
  stringsAsFactors = FALSE
)


design.prob = data.frame(part = 1:10 / 10)



addExperiments(algo.designs = list(filter.all = design.filter.all),
  prob.designs = list(partP = design.prob, partN = design.prob),
  repls = 10)

addExperiments(algo.designs = list(filter.part = design.filter.part),
  prob.designs = list(all = data.frame()),
  repls = 10)



ids.filter.all = findExperiments(algo.name = "filter.all")
ids.filter.part = findExperiments(algo.name = "filter.part")

