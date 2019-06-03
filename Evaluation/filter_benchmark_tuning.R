library(dplyr)
library(BBmisc)
library(data.table)
library(mco)
library(eaf)

load("../Results/results.RData")
load("../Results/data_info.RData")
load("../Results/results_filter_times.RData")

##########################################################################################
# tuning: select best configuration (repl) based on accuracy
# solve ties by selecting the one with fastest runtime

results.filter.times = results.filter.times[, c("dataname", "repl", "fw.method", "iter", "time.filter.outer"), with = FALSE]

results = merge(results, results.filter.times, all.x = TRUE,
  by = c("dataname", "repl", "fw.method", "iter"))
results[is.na(fw.method), ]$time.filter.outer = 0

mmce.inner.best = results[, list(mmce.inner.mean = min(mmce.inner.mean)),
  by = c("algo.name", "fw.method", "iter", "dataname")]

results.tuned = merge(results, mmce.inner.best,
  by = intersect(colnames(results), colnames(mmce.inner.best)))

# detect and resolve ties
ties = results.tuned[, .N, by = c("algo.name", "fw.method", "iter", "dataname")]
ties = ties[N > 1, ]
if (nrow(ties) > 0) {
  remove.rows = numeric(0)
  set.seed(1223)
  for (i in 1:nrow(ties)) {
    if (is.na(ties$fw.method[i])) {
      ids = which(
        results.tuned$algo.name == ties$algo.name[i] &
          is.na(results.tuned$fw.method) &
          results.tuned$dataname == ties$dataname[i] &
          results.tuned$iter == ties$iter[i]
      )
    } else {
      ids = which(
        results.tuned$algo.name == ties$algo.name[i] &
          results.tuned$fw.method == ties$fw.method[i] &
          results.tuned$dataname == ties$dataname[i] &
          results.tuned$iter == ties$iter[i]
      )
    }

    part = results.tuned[ids, ]

    min.time = min(part$timetrain.inner.median)
    min.part.id = which(part$timetrain.inner.median == min.time)
    if (length(min.part.id) > 1) {
      min.part.id = sample(min.part.id, 1)
    }

    remove.new = ids[-min.part.id]
    remove.rows = c(remove.rows, remove.new)
  }
  results.tuned = results.tuned[-remove.rows, ]
}

# when all filter experiments are done, there should be a 10 in each entry
table(results.tuned[, c("fw.method", "dataname")], useNA = "ifany")

# aggregate the outer performance measures for each method
results.tuned.aggr = results.tuned[, list(
  mmce.outer.mean = mean(mmce.outer),
  timetrain.outer.median = median(timetrain.outer),
  time.filter.outer.median = median(as.numeric(time.filter.outer))),
  by = c("algo.name", "fw.method", "dataname")]

# kknn without filtering sometimes produces runtimes of 0:
# add small constant to all median runtimes
results.tuned.aggr$timetrain.outer.median = results.tuned.aggr$timetrain.outer.median + 1e-6

save(results.tuned, results.tuned.aggr, file = "../Results/results_tuning.RData")
