source("function.R")

ratio <- c("1/4", "1/3", "1/2", "3/4", "1", "4/3", "2", "3", "4")
s1 <- c(1/4, 1/3, 1/2, 3/4, 1, 4/3, 2, 3, 4)
s2 <- 1
n_iter <- 10000
seeds <- c(1, 77, 1234)


## start simulation
n1 <- 3; n2 <- 3
walk(seeds, ~ test_simulation(n1, n2, s1, s2, n_iter, seed = .))

n1 <- 4; n2 <- 4
walk(seeds, ~ test_simulation(n1, n2, s1, s2, n_iter, seed = .))

n1 <- 5; n2 <- 5
walk(seeds, ~ test_simulation(n1, n2, s1, s2, n_iter, seed = .))

n1 <- 6; n2 <- 6
walk(seeds, ~ test_simulation(n1, n2, s1, s2, n_iter, seed = .))

n1 <- 3; n2 <- 4
walk(seeds, ~ test_simulation(n1, n2, s1, s2, n_iter, seed = .))

n1 <- 3; n2 <- 5
walk(seeds, ~ test_simulation(n1, n2, s1, s2, n_iter, seed = .))

n1 <- 4; n2 <- 5
walk(seeds, ~ test_simulation(n1, n2, s1, s2, n_iter, seed = .))


## plot figures
dat <- dir(path = "csv", pattern = "*.csv") %>%
    tibble(filename = .) %>%
    mutate(result = map(filename,
                        ~ readr::read_csv(paste0("csv/", .))),
           p = map2(filename, result, plot_sim, s1, ratio))

## save figures
dat %>%
    transmute(png = walk2(filename, p, save_plot_sim))

