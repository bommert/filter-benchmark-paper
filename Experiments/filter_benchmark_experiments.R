library(batchtools)
library(dplyr)
library(BBmisc)
library(mlr)

reg = makeExperimentRegistry(file.dir = "fb", packages = "mlr")

###################################################################################
#### Definition of problem

configs = function(job, data, nr, iter) {
  source("paramset.R")
  set.seed(job$repl * 1000 + nr * 10 + iter - 1)
  pars = sampleValue(par = ps, trafo = TRUE)

  inner = data[[nr]]$inner[[iter]]
  outer = data[[nr]]$outer

  task.outer = data[[nr]]$task
  inds.inner = outer$train.inds[[iter]]
  task.inner = makeClassifTask(data = getTaskData(task.outer)[inds.inner, ],
    target = getTaskTargetNames(task.outer))

  eval = makeFixedHoldoutInstance(
    train.inds = outer$train.inds[[iter]],
    test.inds = outer$test.inds[[iter]],
    size = getTaskSize(task.outer)
  )
  return(list(task.outer = task.outer, task.inner = task.inner,
    cv.inner = inner, eval = eval, pars = pars))
}

# source("task_generation.R")
load("tasksAndRins.RData")

addProblem(name = "all.tasks", seed = 1,
  data = tasksAndRins,
  fun = configs
)



####################################################################################
### Definition of algorithms


# filter + classification
addAlgorithm(
  name = "filter",
  fun = function(job, data, instance, fw.method) {

    source("make_lrn.R")
    source("resampling.R")

    lrn = makeLrn(instance$pars)
    lrn = makeFilterWrapper(learner = lrn, fw.method = fw.method, fw.perc = instance$pars$fw.perc)

    res = resampling(lrn = lrn, instance = instance)
    return(list(res = res, pars = instance$pars))
  }
)


# classification with all features
addAlgorithm(
  name = "no.filter",
  fun = function(job, data, instance) {

    source("make_lrn.R")
    source("resampling.R")

    lrn = makeLrn(instance$pars)

    res = resampling(lrn = lrn, instance = instance)
    return(list(res = res, pars = instance$pars))
  }
)


# filter scores only
addAlgorithm(
  name = "ranking",
  fun = function(job, data, instance, fw.method) {

    source("config.R")

    # for some praznik filters the scores of features which are selected later
    # may be greater than the scores of features which are selected earlier
    pr = grep("praznik", fw.method)

    if (length(pr) > 0) {
      library(praznik)
      praznik.name = strsplit(fw.method, ".", fixed = TRUE)[[1]][2]
      method = get(praznik.name)
      data = getTaskData(instance$task.outer, target.extra = TRUE)

      nf = getTaskNFeats(instance$task.outer)
      res = method(X = data$data, Y = data$target, k = nf)

      vec = rbind(res$selection, length(res$selection):1)

      if (length(res$selection) != nf) {
        omitted = setdiff(1:nf, res$selection)
        names(omitted) = getTaskFeatureNames(instance$task.outer)[omitted]
        vec = cbind(vec, rbind(omitted, NA_real_))
      }

      o = order(vec[1, ])
      vec = vec[, o]
      scores = vec[2, ]
      names(scores) = colnames(vec)

    } else {
      v = generateFilterValuesData(task = instance$task.outer, method = fw.method)
      scores = v$data[[fw.method]]
    }

    return(list(scores = scores))
  }
)


# filter times only
addAlgorithm(
  name = "filter.times",
  fun = function(job, data, instance, fw.method) {

    source("config.R")

    task.dat = getTaskData(instance$task.outer, subset = instance$eval$train.inds[[1]])
    task = makeClassifTask(data = task.dat, target = getTaskTargetNames(instance$task.outer))

    fw.perc = instance$pars$fw.perc
    nselect = round(fw.perc * getTaskNFeats(task))

    t1 = Sys.time()
    v = generateFilterValuesData(task = task, method = fw.method, nselect = nselect)
    t2 = Sys.time()

    dt = difftime(t2, t1, units = "secs")
    return(list(time.filter.outer = dt))
  }
)

#####################################################################################
### Definition of designs

### Design for problems
design.tasks = expand.grid(
  nr = seq_along(tasksAndRins),
  iter = 1:10
)

design.tasks.ranking = expand.grid(
  nr = seq_along(tasksAndRins),
  iter = 1
)


### Desgin for algorithms


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


design.ranking = expand.grid(
  fw.method = filters,
  stringsAsFactors = FALSE
)

design.filter = design.ranking

design.no.filter = data.frame()

design.filter.times = expand.grid(
  fw.method = filters,
  stringsAsFactors = FALSE
)


#################################################################################
### Add Experiments

addExperiments(
  algo.designs = list(ranking = design.ranking),
  prob.designs = list(all.tasks = rbind(design.tasks.ranking)),
  repls = 1)


addExperiments(
  algo.designs = list(filter = design.filter),
  prob.designs = list(all.tasks = design.tasks),
  repls = 100)

addExperiments(
  algo.designs = list(no.filter = design.no.filter),
  prob.designs = list(all.tasks = design.task),
  repls = 100)


addExperiments(
  algo.designs = list(filter.times = design.filter.times),
  prob.designs = list(all.tasks = design.tasks),
  repls = 100)


ids.ranking = findExperiments(algo.name = "ranking")
ids.filter = findExperiments(algo.name = "filter")
ids.no.filter = findExperiments(algo.name = "no.filter")
ids.filter.times = findExperiments(algo.name = "filter.times")
