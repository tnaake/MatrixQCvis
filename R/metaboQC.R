# 
# se <- readRDS("~/SummarizedExperiment_preanalytical.rds")
# 
# 
# ## create columns mz, mz_dev, rt, rt_dev
# n <- nrow(se)
# rowData(se)$mz <- runif(n, min = 100, max = 1000)
# rowData(se)$mz_dev <- rnorm(n, mean = 0, sd = 0.01)
# rowData(se)$rt <- runif(n, min = 10, max = 12000)
# rowData(se)$rt_dev <- rnorm(n, mean = 0, sd = 1)
# 
# 
# devianceHist <- function(se, dev_vals) {
#     dev <- rowData(se)[[dev_vals]]
#     df <- data.frame(dev = dev)
#     ggplot(df, aes_string(x = "dev")) + 
#         geom_histogram(color = "black", fill = "white") + 
#         xlab(dev) + ylab("Number of features") +
#         theme_bw()
# }
# 
# devianceValsScatter <- function(se, vals, dev_vals) {
#     vals_x <- rowData(se)[[vals]]
#     dev_y <- rowData(se)[[dev_vals]]
#     df <- data.frame(vals = vals_x, dev = dev_y)
#     ggplot(df, aes_string(x = "vals", y = "dev")) +
#         geom_point() +
#         xlab(vals) + ylab(dev_vals) +
#         theme_bw()
# }
