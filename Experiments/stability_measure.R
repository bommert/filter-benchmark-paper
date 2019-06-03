SC <- function(features, p, ...)
{
  library(checkmate)

  # make sure that input has desired form
  assertList(x = features, types = "character", min.len = 1)
  all_selected_features <- unique(unlist(features))
  assertInt(p, lower = length(all_selected_features))

  # number of feature sets
  n <- length(features)

  # write feature sets in matrix coded by 0 and 1
  feat_mat <- matrix(0L, ncol = n, nrow = p)
  for (i in 1:n) {
    feature_indices <- which(all_selected_features %in% features[[i]])
    feat_mat[feature_indices, i] <- 1L
  }

  # calculate correlations
  cors <- unlist(sapply(1:(n - 1), function(i) {
    sapply((i + 1):n, function(j) {
      cor_value <- suppressWarnings(cor(feat_mat[, i], feat_mat[, j]))
      if (is.na(cor_value)) {
        if (all(feat_mat[, i] == feat_mat[, j])) {
          cor_value <- 1
        } else {
          cor_value <- -1
        }
      }
      cor_value
    })
  }))

  cor_pearson <- mean(cors)
  cor_pearson
}
