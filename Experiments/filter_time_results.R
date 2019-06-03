library(BBmisc)
library(batchtools)
library(checkmate)
library(dplyr)


reg.dir = choose.dir()
loadRegistry(reg.dir, writeable = TRUE, work.dir = getwd())

fun = function(job, res) {
  data.table(
    algo.name = job$algo.name,
    prob.name = job$prob.name,
    algo.pars = list(job$algo.pars),
    prob.pars = list(job$prob.pars),
    repl = job$repl,
    time = list(res))
}

results.time = reduceResultsDataTable(findDone(), fun = fun)
results.time = bind_rows(results.time$result)
results.time = unwrap(results.time, cols = c("algo.pars", "prob.pars"))

results.time = results.time[, list(time = list(unlist(time))),
  by = c("algo.name", "prob.name", "fw.method", "fw.perc", "part")]

save(results.time, file = "../Results/results_time.RData")

