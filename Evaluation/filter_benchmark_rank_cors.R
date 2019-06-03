library(ggplot2)
library(data.table)
library(BBmisc)

load("../Results/results_ranking.RData")
load("../Results/data_n_feats.RData")

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

results.ranking$fw.method = rename.filters(results.ranking$fw.method)


############################################################################
# matrices of rank correlations between the scores of the different filters
# one matrix per dataset

filters = sort(unique(results.ranking$fw.method))


cor.fun = function(x, names) {
  x = lapply(x, as.numeric)
  z = t(convertListOfRowsToDataFrame(x))
  co = cor(z, method = "spearman", use = "pairwise.complete.obs")
  colnames(co) = rownames(co) = names

  # add NA columns and rows for missing results
  missing = setdiff(filters, names)
  n.missing = length(missing)
  if (n.missing > 0) {
    na.cols = matrix(NA_real_, nrow = nrow(co), ncol = n.missing)
    colnames(na.cols) = missing
    co = cbind(co, na.cols)
    na.rows = matrix(NA_real_, nrow = n.missing, ncol = ncol(co))
    co = rbind(co, na.rows)
    rownames(co) = colnames(co)
  }
  co = co[filters, filters]
  return(co)
}

rank.cors = results.ranking[, list(cor = list(cor.fun(scores, fw.method))), by = "dataname"]

rank.cors.nfeats = merge(rank.cors,
  data.table(dataname.nfeats = data.n.feats, dataname = names(data.n.feats)),
  by = "dataname", sort = FALSE)
# rank.cors.nfeats = rank.cors.nfeats[new.order, ]


mc = function(cors) {
  mean.cors = matrix(0, nrow = length(filters), ncol = length(filters))
  for (i in seq_along(filters)) {
    for (j in seq_along(filters)) {
      values = unlist(lapply(cors, function(x) x[i, j]))
      mean.cors[i, j] = mean(values, na.rm = TRUE)
    }
  }

  colnames(mean.cors) = rownames(mean.cors) = filters

  hc = hclust(as.dist(1 - mean.cors), method = "average")
  o = rev(hc$order)

  dendro = as.dendrogram(hc)

  return(list(mean.cors = mean.cors, o = o))
}


mean.cor = mc(rank.cors$cor)


plot_mat = function(dat, names = filters, title = "") {
  colnames(dat) = rownames(dat) = names
  plt.data = cbind(measure = names, as.data.frame(dat))
  plt.data = melt(plt.data, id.vars = "measure")
  plt.data$measure = factor(plt.data$measure, levels = names)
  plt.data$variable = factor(plt.data$variable, levels = names)

  gg = ggplot(plt.data, aes(measure, variable)) +
    geom_tile(aes(fill = value), colour = "white") +
    scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred", limits = c(-1, 1), name = "Similarity") +
    theme_grey() +
    labs(x = "", y = "", title = title) +
    scale_x_discrete(expand = c(0, 0), labels = names) +
    scale_y_discrete(expand = c(0, 0), labels = names) +
    theme(axis.ticks = element_blank()) +
    coord_equal(ratio = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12))

  print(gg)

  return(NULL)
}

pdf("../Plots/rank_cor.pdf", width = 7.5, height = 7)
lapply(seq_along(rank.cors.nfeats$cor), function(i) {
  cor = rank.cors.nfeats$cor[[i]]
  o = mean.cor$o
  names.o = colnames(cor)[o]
  plot_mat(cor[o, o], title = rank.cors.nfeats$dataname[i], names = names.o)
})
dev.off()

pdf("../Plots/rank_cor_mean.pdf", width = 7.5, height = 7)
m.cor = mean.cor$mean.cors
o = mean.cor$o
names.o = colnames(m.cor)[o]
plot_mat(m.cor[o, o], names = names.o)
dev.off()

round(mean(mean.cor$mean.cors), 4)
round(mean.cor$mean.cors, 4)

save(mean.cor, file = "mean_cor.RData")
