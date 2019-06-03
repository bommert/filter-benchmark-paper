library(xtable)
library(mlr)

load("../Results/data_info.RData")
nrs = c(2, 4, 5, 6, 7, 8, 10, 11, 13, 14, 15, 16, 17, 18, 20, 21)
data.info = data.info[nrs, ]

dat = data.info[, c("name", "n.feats", "n.obs", "majority.perc")]
dat$majority.perc = round(dat$majority.perc, 2)

o = order(dat$n.feats)
dato = dat[o, ]
datox = xtable(dato)
print(datox, include.rownames = FALSE)



lf = listFilterMethods(tasks = TRUE, features = TRUE)
lf = lf[lf$task.classif & lf$feature.numerics, ]

lfx = xtable(lf[ , 1:2])
print(lfx, include.rownames = FALSE)
