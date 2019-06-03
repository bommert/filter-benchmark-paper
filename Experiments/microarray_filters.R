
#' Filter \dQuote{limma} builds a linear model and
#' selects the features with the largest absolute
#' values of the moderated F-statistic.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "limma",
  desc = "Moderated t-statistic",
  pkg  = c("limma", "gtools"),
  supported.tasks = "classif",
  supported.features = "numerics",
  fun = function(task, nselect, ...) {
    data = t(getTaskData(task, target.extra = TRUE)$data)
    target = getTaskData(task, target.extra = TRUE)$target

    # solve naming problems
    n = length(levels(target))
    levels(target) = paste0("C", seq_len(n))

    design = model.matrix(~ 0 + target)
    colnames(design) = levels(target)
    fit = limma::lmFit(data, design)

    ctrs = gtools::combinations(length(levels(target)), 2, levels(target))
    ctrs = apply(ctrs, 1, function(x) paste(x, collapse = " - "))
    cont.matrix = limma::makeContrasts(contrasts = ctrs, levels = design)
    mod = limma::contrasts.fit(fit, cont.matrix)

    fitB = limma::eBayes(mod)
    tt = limma::topTableF(fitB, number = getTaskNFeats(task), sort = "none")
    res = tt$F
    names(res) = rownames(tt)
    res
  }
)


#' Filter \dQuote{sam} performs an significance analysis of microarrays
#' and selects the features with the largest absolute
#' values of the moderated t-statistic.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "sam",
  desc = "Moderated t-statistic",
  pkg  = c("samr", "impute"),
  supported.tasks = "classif",
  supported.features = "numerics",
  fun = function(task, nselect, ...) {
    data = t(getTaskData(task, target.extra = TRUE)$data)
    target = getTaskData(task, target.extra = TRUE)$target

    if (length(levels(target)) > 2L) {
      resp.type = "Multiclass"
    } else {
      resp.type = "Two class unpaired"
    }

    target = as.numeric(target)

    sam.out = samr::SAM(x = data, y = target, resp.type = resp.type)

    scores = abs(sam.out$samr.obj$tt)
    names(scores) = getTaskFeatureNames(task)
    scores
  }
)
