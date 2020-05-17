library(coin)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

test_simulation <- function(n1, n2, sigma1, sigma2, n_iter, seed = NULL) {
    if (!is.null(seed)) {
        set.seed(seed)
    } else {
        seed <- 0
    }

    n_test <- 4
    n_sigma <- length(sigma1)
    pSim <- matrix(numeric(n_iter * n_test), ncol = n_test)
    Error <- matrix(numeric(n_sigma * n_test), ncol = n_test)

    for (i in seq_len(n_sigma)) {
        for (j in seq_len(n_iter)) {
            ## set data
            x1 <- rnorm(n1, mean = 10, sd = sigma1[i])
            x2 <- rnorm(n2, mean = 10, sd = sigma2)
            dat <- data.frame(
                group = rep(c("x1", "x2"), c(n1, n2)),
                value = c(x1, x2)
            )
            ## get P values
            pSim[j, 1] <- t.test(x1, x2, var.equal = TRUE)$p.value
            pSim[j, 2] <- t.test(x1, x2)$p.value
            pSim[j, 3] <- pvalue(wilcox_test(value ~ group, data = dat))
            pSim[j, 4] <- pvalue(wilcox_test(value ~ group,
                                             distribution = "exact", data = dat))
        }
        ## calculate Type I Error
        Error[i,] <- colMeans(pSim < 0.05)
    }

    res <- data.frame(cbind(sigma1, Error))
    names(res) <- c("ratio", "t", "Welch", "Wilcoxon", "Wilcoxon(exact)")

    dir.create("csv", showWarnings = FALSE, recursive = TRUE)
    write.csv(res, row.names = FALSE,
              file = sprintf("csv/result_%d_%d_%05d.csv", n1, n2, seed))
}


plot_sim <- function(filename, result, sigma1, ratio) {
    res_long <- result %>%
        pivot_longer(cols = -ratio,
                     names_to = "Method", values_to = "Error")

    stlip_name <- stringr::str_split(filename, "_|\\.", simplify = TRUE)
    n1 <- stlip_name[2]
    n2 <- stlip_name[3]
    seed <- stlip_name[4] %>% as.numeric()

    p <- ggplot(res_long,
                aes(ratio, Error,
                    group = Method, shape = Method, color= Method)) +
        geom_line() +
        geom_point(size = 2) +
        ylim(0, 0.2) +
        scale_x_log10(breaks = sigma1, labels = ratio) +
        labs(x = "Sigma1 / Sigma2",
             y = "Type I error") +
        ggtitle(sprintf("n1 = %s, n2 = %s (seed = %d)", n1, n2, seed)) +
        theme(legend.position = "right",
              panel.grid.minor = element_blank(),
              text = element_text(size = 16))

}

save_plot_sim <- function(filename, p,
                          width = 480, height = 300) {
    filename <- paste0("png/",
                       stringr::str_replace(filename, "\\.csv", "\\.png"))
    dir.create("png", showWarnings = FALSE, recursive = TRUE)
    ggsave(filename, p, width = width / 72, height = height / 72)
}

