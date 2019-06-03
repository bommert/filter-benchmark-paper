library(OpenML)
library(datamicroarray)
library(dplyr)
library(BBmisc)
library(mlr)

set.seed(123)

# Generate list of tasks:
# retrieve data from Open ML
# and include manually downloaded data
# as well as data from the microarray package
taskList = function() {

  # l = listOMLDataSets(
  #   number.of.classes = 2L,
  #   number.of.features = c(200L, 20000L),
  #   number.of.instances = c(200L, 200000L),
  #   number.of.missing.values = 0L
  # )
  #
  # l = l[l$number.of.features < l$number.of.instances, ]
  #
  # n.binary = unlist(lapply(l$data.id, function(id) {
  #   info = getOMLDataSetQualities(id)
  #   info$value[info$name == "NumberOfBinaryFeatures"]
  # }))
  # l = cbind(l, number.of.binary.features = n.binary)
  #
  # l = l[l$number.of.symbolic.features == 1L | l$number.of.symbolic.features == l$number.of.binary.features, ]


  ##########
  # Query to OpenML does not deliver same result anymore!
  # For reproducibility, manually list the datasets that the query used to deliver
  nms = c("IMDB.drama", "scene", "mfeat-factors", "gina_agnostic", "hiva_agnostic",
    "gina_prior", "madelon", "Bioresponse", "Speech", "Internet-Advertisements", "gisette")
  ids = c(273, 312, 978, 1038,  1039, 1042, 1485, 4134, 40910, 40978, 41026)
  l = lapply(nms, function(n) listOMLDataSets(data.name = n))
  l = dplyr::bind_rows(l)
  l = l[l$data.id %in% ids, ]


  # get datasets from openml
  # datasets = lapply(l$data.id, getOMLDataSet)
  # work around java heapsize problems
  datasets = lapply(seq_row(l), function(i) {
    if (l$format[i] == "Sparse_ARFF") {
      setOMLConfig(arff.reader = "RWeka")
      gc()
    } else {
      setOMLConfig(arff.reader = "farff")
    }
    getOMLDataSet(l$data.id[i])
  })

  # remove datasets that do not have exactly one target
  n.targets = unlist(lapply(datasets, function(x) length(x$target.features)))
  one.target = which(n.targets == 1L)

  datasets = datasets[one.target]
  l = l[one.target, ]

  data.info = l

  # create mlr tasks removing features that should be ignored
  tasks = lapply(datasets, convertOMLDataSetToMlr)

  # convert factors to numerics (they must be binary, see above)
  feats = lapply(tasks, function(x) x$task.desc$n.feat)
  feats = convertListOfRowsToDataFrame(feats)
  factors = which(feats$factors > 0L)
  if (length(factors) > 0L) {
    for (f in factors) {
      dat = getTaskData(tasks[[f]], target.extra = TRUE)
      types = unlist(lapply(dat$data, typeof))
      factors.inds = which(types != "double")
      dat$data[factors.inds] = lapply(dat$data[factors.inds], as.numeric)
      new.task = makeClassifTask(data = cbind(dat$data, target = dat$target),
        target = "target")
      tasks[[f]] = new.task
    }
  }

  ##############################################################################################

  # Include dataset manually from UCI

  # Create mlr task for UJIndoorLoc data manually
  # based on the data from
  # https://archive.ics.uci.edu/ml/machine-learning-databases/00310/

  uj1 = read.csv("UJIndoorLoc/UJIndoorLoc/trainingData.csv")
  uj2 = read.csv("UJIndoorLoc/UJIndoorLoc/validationData.csv")
  uj = rbind(uj1, uj2)

  table(uj$BUILDINGID)/nrow(uj)

  # target variables: latitude, longitude, floor and Building ID
  # supplemenetary information: space ID, relative position, user ID, phone ID and timestamp

  # use only WAP data to predict Building ID
  # combine classes 0 and 1

  class = ifelse(uj$BUILDINGID == 2, 1, -1)
  uj.part = cbind(Class = as.factor(class), uj[, 1:520])

  uj.task = makeClassifTask(id = "UJIndoorLoc", data = uj.part, target = "Class")


  tasks = c(tasks, list(uj.task))
  l3 = data.frame(name = "UJIIndoorLoc", stringsAsFactors = FALSE)

  ##################################################################################################

  # data from package datamicroarray
  # we cannot rely on the information of describe_data() for the moment
  dd = describe_data()
  # select = dd$n >= 100 & dd$K == 2
  # ds = as.character(dd[select, ]$author)
  ds = as.character(dd$author)
  data(list = ds)

  tasks.dma = lapply(ds, function(d) {
    x = get(d)
    x$x = as.matrix(x$x)

    # use largest and second largest class if they have at least 100 obs
    tab = table(x$y)
    s.tab = sort(tab, decreasing = TRUE)
    n = sum(s.tab[1:2])

    if (n >= 100) {
      if (length(tab) > 2) {
        ids = which(x$y %in% names(s.tab)[1:2])
        x$x = x$x[ids, ]
        x$y = droplevels(x$y[ids])
      }

      # normalize if neccessary
      if (max(x$x, na.rm = TRUE) > 30) {
        if (min(x$x, na.rm = TRUE) <= -1) {
          return(NULL)
        } else {
          x$x = log10(x$x + 1)
        }
      }

      # remove constant features
      mins = apply(x$x, 2, min, na.rm = TRUE)
      maxs = apply(x$x, 2, max, na.rm = TRUE)
      eq = maxs == mins
      if (any(eq)) {
        x$x = x$x[, !eq]
      }

      colnames(x$x) = paste0("V", seq_col(x$x))

      dat = cbind(data.frame(Class = x$y), x$x)

      # remove observations with missing values
      nas = apply(dat, 1, function(x) any(is.na(x)))
      if (any(nas)) {
        dat = dat[!nas, ]
      }

      task = makeClassifTask(id = d, data = dat, target = "Class")
      return(task)
    } else {
      return(NULL)
    }
  })

  tasks.dma = tasks.dma[!unlist(lapply(tasks.dma, is.null))]
  ds = unlist(lapply(tasks.dma, getTaskId))


  tasks = c(tasks, tasks.dma)

  l4 = data.frame(name = ds, stringsAsFactors = FALSE)
  data.info = bind_rows(data.info, l3, l4)

  # save for mapping datasets to nr
  n.feats = unlist(lapply(tasks, getTaskNFeats))
  n.obs = unlist(lapply(tasks, getTaskSize))
  majority.perc = unlist(lapply(tasks, function(x) {
    cd = getTaskDesc(x)$class.distribution
    max(cd) / sum(cd)
  }))
  data.info = cbind(data.info, n.feats = n.feats, n.obs = n.obs,
    majority.perc = majority.perc)

  save(data.info, file = "data_info.RData")

  return(tasks)
}

tasks = taskList()


# list of resampling instances with inner and outer resampling
tasksAndRins = lapply(tasks, function(task) {
  rdesc = makeResampleDesc(method = "CV", stratify = TRUE, iters = 10)

  outer = makeResampleInstance(desc = rdesc, task = task)

  inner = lapply(outer$train.inds, function(inds) {
    part.task = makeClassifTask(data = getTaskData(task)[inds, ],
      target = getTaskTargetNames(task))
    makeResampleInstance(desc = rdesc, task = part.task)
  })

  return(list(task = task, outer = outer, inner = inner))
})

save(tasksAndRins, file = "tasksAndRins.RData")
