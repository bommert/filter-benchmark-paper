library(BBmisc)
library(batchtools)
library(checkmate)
library(dplyr)

load("../Results/data_info.RData")

# enter directory of batchtools registry
reg.dir = choose.dir()
loadRegistry(reg.dir, writeable = TRUE, work.dir = getwd())

# only the datasets that finished in time
nrs = c(2, 4, 5, 6, 7, 8, 10, 11, 13, 14, 15, 16, 17, 18, 20, 21)
all = unwrap(getJobTable(findDone()))
all = all[nr %in% nrs, ]

ids.ranking = all[algorithm == "ranking", ]
ids.filter = all[algorithm == "filter", ]
ids.no.filter = all[algorithm == "no.filter", ]
ids.filter.times = all[algorithm == "filter.times", ]

data.info.original = data.info
data.info = data.info[nrs, ]
new.order = order(data.info$n.feats)

data.n.feats = paste0(data.info$name, " (", data.info$n.feats, ")")
names(data.n.feats) = data.info$name
save(data.n.feats, file = "../Results/data_n_feats.RData")


#################################################################################
# filter
# reduce and aggregate in parts because of memory consumption

fun.filter = function(job, res) {
  data.table(
    algo.name = job$algo.name,
    prob.name = job$prob.name,
    algo.pars = list(job$algo.pars),
    prob.pars = list(job$prob.pars),
    repl = job$repl,
    hyperpars = list(res$pars),
    measures.test.inner = list(res$res$measures.test.inner),
    measures.test.outer = list(res$res$measures.test.outer)
    #features.outer = list(res$res$features.outer)
  )
}

reduce.and.aggregate.filter = function(ids) {
  red.ids = findDone(ids)

  # for intermediate analyses while jobs are still running on cluster
  exist.ids = file.exists(paste0(reg.dir, "/results/", red.ids$job.id, ".rds"))
  red.ids = red.ids[exist.ids, ]

  res = reduceResultsDataTable(red.ids, fun = fun.filter)

  unwrap.cols = c("algo.pars", "prob.pars", "measures.test.outer")
  remove.cols = c("measures.test.outer.iter", "measures.test.inner.iter")

  res2 = lapply(res$result, function(r) {
    r2 = unwrap(r, cols = unwrap.cols, sep = ".")

    part = r2[, "measures.test.inner"][[1]][[1]]
    part = as.data.table(part)
    part = part[, list(measures.test.inner.mmce = mmce, measures.test.inner.timetrain = timetrain)]

    rest = r2[, -"measures.test.inner"]
    rests = lapply(seq_len(nrow(part)), function(i) rest)
    rest = bind_rows(rests)

    r3 = cbind(rest, part)
    results.names = setdiff(colnames(r3), remove.cols)
    r3[, results.names, with = FALSE]
  })
  results = bind_rows(res2)


  # renaming
  colnames(results)[colnames(results) == "algo.pars.fw.method"] = "fw.method"
  colnames(results)[colnames(results) == "prob.pars.nr"] = "nr"
  colnames(results)[colnames(results) == "prob.pars.iter"] = "iter"
  colnames(results)[colnames(results) == "measures.test.outer.mmce"] = "mmce.outer"
  colnames(results)[colnames(results) == "measures.test.inner.mmce"] = "mmce.inner"
  colnames(results)[colnames(results) == "measures.test.outer.timetrain"] = "timetrain.outer"
  colnames(results)[colnames(results) == "measures.test.inner.timetrain"] = "timetrain.inner"

  results = results[ , list(
    mmce.inner.mean = mean(mmce.inner),
    mmce.inner.median = median(mmce.inner),
    timetrain.inner.mean = mean(timetrain.inner),
    timetrain.inner.median = median(timetrain.inner),
    mmce.outer = unique(mmce.outer),
    timetrain.outer = unique(timetrain.outer)
  ), by = c("algo.name", "prob.name", "fw.method", "nr", "repl", "iter")]

  return(results)
}


tab.parts = ids.filter[, list(ids = list(job.id)), by = "nr"]

aggr.parts = lapply(1:nrow(tab.parts), function(i) {
  reduce.and.aggregate.filter(unlist(tab.parts$ids[i]))
})

results.filter = bind_rows(aggr.parts)


#################################################################################
# no filter
reduce.and.aggregate.no.filter = function(ids) {
  red.ids = findDone(ids)

  # for intermediate analyses while jobs are still running on cluster
  exist.ids = file.exists(paste0(reg.dir, "/results/", red.ids$job.id, ".rds"))
  red.ids = red.ids[exist.ids, ]

  res = reduceResultsDataTable(red.ids, fun = fun.filter)

  unwrap.cols = c("algo.pars", "prob.pars", "measures.test.outer")
  remove.cols = c("measures.test.outer.iter", "measures.test.inner.iter")

  res2 = lapply(res$result, function(r) {
    r2 = unwrap(r, cols = unwrap.cols, sep = ".")

    part = r2[, "measures.test.inner"][[1]][[1]]
    part = as.data.table(part)
    part = part[, list(measures.test.inner.mmce = mmce, measures.test.inner.timetrain = timetrain)]

    rest = r2[, -"measures.test.inner"]
    rests = lapply(seq_len(nrow(part)), function(i) rest)
    rest = bind_rows(rests)

    r3 = cbind(rest, part)
    results.names = setdiff(colnames(r3), remove.cols)
    r3[, results.names, with = FALSE]
  })
  results = bind_rows(res2)


  # renaming
  colnames(results)[colnames(results) == "prob.pars.nr"] = "nr"
  colnames(results)[colnames(results) == "prob.pars.iter"] = "iter"
  colnames(results)[colnames(results) == "measures.test.outer.mmce"] = "mmce.outer"
  colnames(results)[colnames(results) == "measures.test.inner.mmce"] = "mmce.inner"
  colnames(results)[colnames(results) == "measures.test.outer.timetrain"] = "timetrain.outer"
  colnames(results)[colnames(results) == "measures.test.inner.timetrain"] = "timetrain.inner"

  results = results[ , list(
    mmce.inner.mean = mean(mmce.inner),
    mmce.inner.median = median(mmce.inner),
    timetrain.inner.mean = mean(timetrain.inner),
    timetrain.inner.median = median(timetrain.inner),
    mmce.outer = unique(mmce.outer),
    timetrain.outer = unique(timetrain.outer)
  ), by = c("algo.name", "prob.name", "nr", "repl", "iter")]

  return(results)
}

results.no.filter = reduce.and.aggregate.no.filter(ids.no.filter)

results = bind_rows(results.filter, results.no.filter)

results = merge(results,
  data.table(nr = 1:nrow(data.info.original), dataname = data.info.original$name), by = "nr")

results$dataname = droplevels(factor(results$dataname,
  levels = data.info$name[new.order]))

save(results, file = "../Results/results.RData")

#################################################################################
# ranking

fun.ranking = function(job, res) {
  data.table(
    algo.name = job$algo.name,
    prob.name = job$prob.name,
    algo.pars = list(job$algo.pars),
    prob.pars = list(job$prob.pars),
    scores = list(res$scores)
  )
}

reduce.and.aggregate.ranking = function(ids) {
  res = reduceResultsDataTable(ids, fun = fun.ranking)
  res = bind_rows(res$result)
  results = unwrap(res , cols = c("algo.pars", "prob.pars"))
  results = results[, -"iter"]
  return(results)
}

results.ranking = reduce.and.aggregate.ranking(ids.ranking)

results.ranking = merge(results.ranking,
  data.table(nr = 1:nrow(data.info.original), dataname = data.info.original$name), by = "nr")

results.ranking$dataname = droplevels(factor(results.ranking$dataname,
  levels = data.info$name[new.order]))

save(results.ranking, file = "../Results/results_ranking.RData")

####################################################################################
# filter times

fun.filter.time = function(job, res) {
  data.table(
    algo.name = job$algo.name,
    prob.name = job$prob.name,
    algo.pars = list(job$algo.pars),
    prob.pars = list(job$prob.pars),
    repl = job$repl,
    time.filter.outer = res$time.filter.outer
  )
}

reduce.and.aggregate.filter.time = function(ids) {
  red.ids = findDone(ids)

  # for intermediate analyses while jobs are still running on cluster
  exist.ids = file.exists(paste0(reg.dir, "/results/", red.ids$job.id, ".rds"))
  red.ids = red.ids[exist.ids, ]

  res = reduceResultsDataTable(red.ids, fun = fun.filter.time)
  res = bind_rows(res$result)

  results = unwrap(res, cols = c("algo.pars", "prob.pars"))
  return(results)
}

results.filter.times = reduce.and.aggregate.filter.time(ids.filter.times)

results.filter.times = merge(results.filter.times,
  data.table(nr = 1:nrow(data.info.original), dataname = data.info.original$name), by = "nr")

results.filter.times$dataname = droplevels(factor(results.filter.times$dataname,
  levels = data.info$name[new.order]))

save(results.filter.times, file = "../Results/results_filter_times.RData")
