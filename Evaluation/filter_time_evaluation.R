library(ggplot2)
library(data.table)
library(ggpubr)

load("../Results/results_time.RData")

# generated in filter_benchmark_rank_cors.R
load("mean_cor.RData")

# rename filters
rename.filters = function(n) {
  n2 = strsplit(n, "FSelectorRcpp.")
  n2a = unlist(lapply(n2, function(x) x[length(x)]))

  n3 = strsplit(n2a, "praznik.")
  n3a = unlist(lapply(n3, function(x) x[length(x)]))

  n4 = strsplit(n3a, "ranger.")
  n4a = unlist(lapply(n4, function(x) x[length(x)]))

  n5 = ifelse(n4a == "gainratio", "gain.ratio", n4a)
  n5 = ifelse(n5 == "infogain", "info.gain", n5)
  n5 = ifelse(n5 == "symuncert", "sym.uncert", n5)

  return(n5)
}

results.time$fw.method = rename.filters(results.time$fw.method)
results.time$fw.method = factor(results.time$fw.method, levels = colnames(mean.cor$mean.cors)[mean.cor$o])

############################################################################################################

# unit: seconds
time.aggr = results.time[, list(
  time.median = median(unlist(time)) / 1e9,
  time.25 = quantile(unlist(time), 0.25) / 1e9,
  time.75 = quantile(unlist(time), 0.75) / 1e9),
  by = c("algo.name", "prob.name", "fw.method", "fw.perc", "part")]

# percentage values from 0 to 100
time.aggr$part = time.aggr$part * 100
time.aggr$fw.perc = time.aggr$fw.perc * 100

filters = unique(time.aggr$fw.method)
linetypes = c("solid", "dashed", "dotted", "twodash", "longdash")
lts = rep(linetypes, ceiling(length(filters) / length(linetypes)))[seq_along(filters)]

filter.time.n = time.aggr[prob.name == "partN", ]
filter.time.p = time.aggr[prob.name == "partP", ]

filter.time.perc = time.aggr[prob.name == "all", ]
filter.time.perc = filter.time.perc[, -"part"]
colnames(filter.time.perc)[colnames(filter.time.perc) == "fw.perc"] = "part"


ggFilterTime = function(data, y.limit = NA, xlab = "") {
  gg = ggplot(data = data,
    mapping = aes(x = part, y = time.median, group = fw.method, col = fw.method, linetype = fw.method)) +
    theme_bw() +
    geom_line(size = 1.1) +
    scale_linetype_manual(values = lts, drop = FALSE, name = "Filter method") +
    scale_color_discrete(drop = FALSE, name = "Filter method") +
    theme(legend.position = "bottom",
      legend.key.width = unit(3, "line"),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.spacing.x = unit(2, "line")) +
    ylab("Median run time (in seconds)") +
    xlab(xlab) +
    guides(linetype = guide_legend(ncol = 3))
  if (!is.na(y.limit)) {
    gg = gg + ylim(0, y.limit)
  }
  return(gg)
}

ggn1 = ggFilterTime(filter.time.n, NA, "Percentage of instances of original dataset")
ggn2 = ggFilterTime(filter.time.n, 55, "Percentage of instances of original dataset")

ggp1 = ggFilterTime(filter.time.p, NA, "Percentage of features of original dataset")
ggp2 = ggFilterTime(filter.time.p, 55, "Percentage of features of original dataset")

ggf1 = ggFilterTime(filter.time.perc, NA, "Percentage of features to be chosen")
ggf2 = ggFilterTime(filter.time.perc, 55, "Percentage of features to be chosen")


pdf("../Plots/filter_time.pdf", width = 14, height = 11)
ggarrange(ggf1, ggf2, ggp1, ggp2, ggn1, ggn2, ncol = 2, nrow = 3,
  common.legend = TRUE, legend = "bottom")
dev.off()
