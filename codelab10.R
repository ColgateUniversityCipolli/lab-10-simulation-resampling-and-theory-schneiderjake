#######################################################################
#Lab 10
#Jake Schneider
#######################################################################
library(tidyverse)
library(patchwork)

##################################################
#Task 1
#################################################
################
#sample size 10000
poll.size <- 1004
poll.probability <- 0.39
poll.generation <- 10000

simulation1.poll.dat <- tibble(
  prop = rbinom(poll.generation, poll.size, poll.probability) / poll.size 
)

ci1 <- quantile(simulation1.poll.dat$prop, probs = c(0.025, 0.975))
moe1 <- (ci1[2] - ci1[1]) / 2


ggplot(simulation1.poll.dat, aes(x = prop)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "blue", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Simulation 1", x = "Percentage", y = "Density")





################
#poll size is 2008
poll.size <- 2008
poll.probability <- 0.39
poll.generation <- 10000

simulation1.poll.dat <- tibble(
  prop = rbinom(poll.generation, poll.size, poll.probability) / poll.size 
)

ggplot(simulation1.poll.dat, aes(x = prop)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "blue", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Sampling Distribution of Sample Mean", x = "Sample Mean", y = "Density")

ci2 <- quantile(simulation1.poll.dat$prop, probs = c(0.025, 0.975))
moe2 <- (ci2[2] - ci2[1]) / 2


##################################################
#Task 2
#################################################
gallup.survery <- tibble(
  reponse = c(
    rep("satisfied", 392),
    rep("dissatisfied", 592),
    rep("no.opinion", 20)
  )
)

R <- 1000
resamples <- tibble(p.hat = numeric(R))

for( i in 1:R){
  # Take a resample
  curr.resample <- sample(x = gallup.survery$reponse,
                          size = nrow(gallup.survery),
                          replace = T)
  # compute the stat on the resample
  resamples$p.hat[i] <- mean(curr.resample == "satisfied")
}


ggplot(resamples, aes(x = p.hat)) + 
  # Histogram underneath
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30, fill = "gray90", color = "gray70") +
  
  # Bar chart of normalized counts on top
  geom_bar(aes(y = after_stat(count / sum(count))), 
           fill = "lightblue", color = "white", width = 0.005) +
  
  # Density curve on top
  geom_density(color = "red", linewidth = 1) +
  
  # Styling
  geom_hline(yintercept = 0) +
  theme_bw() +
  xlab(bquote(hat(p))) +
  ylab("Density") +
  scale_x_continuous(breaks = round(seq(0.3, 0.5, 0.01), 2))

ci3 <- quantile(resamples$p.hat, probs = c(0.025, 0.975))
moe3 <- (ci3[2] - ci3[1]) / 2



##################################################
#Task 3
#################################################
n.values <- seq(100,3000, 10)
p.values <- seq(.01,.99, .01)
simulation.size <- 10000

sim.grid <- expand_grid(n = n.values, p = p.values)

n.p.sim.results <- sim.grid |>
  rowwise() |>
  mutate(
    moe = {
      props <- rbinom(simulation.size, size = n, prob = p) / n
      ci <- quantile(props, probs = c(0.025, 0.975))
      (ci[2] - ci[1]) / 2
    }
  ) |>
  ungroup()


moe.heatmap <- ggplot(n.p.sim.results, aes(x = n, y = p, fill = moe)) +
  geom_raster() +
  scale_fill_viridis_c(option = "plasma", name = "Margin of Error") +
  labs(
    title = "Simulation-Based Margin of Error by n and p",
    x = "Sample Size (n)",
    y = "True Proportion (p)"
  ) +
  theme_minimal()


moe.heatmap.advanced <- ggplot(n.p.sim.results, aes(x = p, y = n, fill = moe)) +
  geom_raster() +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Margin of Error by Sample Size and Proportion",
       x = "True Proportion (p)",
       y = "Sample Size (n)",
       fill = "MOE") +
  theme_minimal() +
  # Add contour lines for specific MOE values
  geom_contour(aes(z = moe), breaks = c(0.02, 0.03, 0.04, 0.05), color = "white") +
  # Add text annotation
  annotate("text", x = 0.5, y = 2800, 
           label = "MOE decreases with sample size and\nis smallest when p is near 0 or 1", 
           color = "white", size = 3.5)

##################################################
#Task 4
#################################################
#wilson moe
wilson.margin <- function(n, p, z = 1.96) {
  numerator <- z * sqrt((p * (1 - p) + z^2/(4*n))/n)
  denominator <- 1 + z^2/n
  return(numerator / denominator)
}

wilson.n.values <- seq(100,2000, 10)
wilson.p.values <- seq(.01,.99, .01)

wilson.sim.grid <- expand_grid(n = wilson.n.values, p = wilson.p.values)

#wilson moe for all of our combos 
wilson.results <- wilson.sim.grid |>
  rowwise() |>
  mutate(
    moe = wilson.margin(n, p)
  ) |>
  ungroup()


wilson.heatmap <- ggplot(wilson.results, aes(x = n, y = p, fill = moe)) +
  geom_raster() +
  scale_fill_viridis_c(option = "plasma", name = "Margin of Error") +
  labs(
    title = "Simulation-Based Wilson Margin of Error by n and p",
    x = "Sample Size (n)",
    y = "True Proportion (p)"
  ) +
  theme_minimal()

wilson.heatmap.advanced <- ggplot(wilson.results, aes(x = p, y = n, fill = moe)) +
  geom_raster() +
  scale_fill_viridis_c(option = "viridis") +
  labs(title = "Wilson Margin of Error by Sample Size and Proportion",
       x = "Proportion (p)",
       y = "Sample Size (n)",
       fill = "MOE") +
  theme_minimal() +
  geom_contour(aes(z = moe), breaks = c(0.02, 0.03, 0.04, 0.05), color = "white") +
  annotate("text", x = 0.5, y = 1800, 
           label = "Wilson method provides\nmore accurate MOE estimates", 
           color = "white", size = 3.5)

results.comparison <- tibble(
  Method = c("Simulation (n=1004)", "Simulation (n=2008)", "Bootstrap (n=1004)", "Wilson (n=1004, p=0.39)"),
  MOE = c(
    moe1,
    moe2,
    moe3,
    wilson.margin(1004, 0.39)
  )
)



##################################################
#optional coding challenge 
#################################################

bootstrap_n.values <- seq(100, 800, 100)  # Using fewer n values
bootstrap_p.values <- seq(0.1, 0.9, 0.1)  # Using fewer p values
bootstrap_sim.count <- 10   # Number of simulations per n,p combination
bootstrap_resample.count <- 100  # Number of bootstrap resamples per simulation

# Create grid of all n,p combinations
bootstrap.grid <- expand_grid(n = bootstrap_n.values, p = bootstrap_p.values)

# Initialize results data frame
bootstrap.results <- bootstrap.grid |>
  rowwise() |>
  mutate(
    moe = {
      # Store MOEs from multiple simulations
      sim_moes <- numeric(bootstrap_sim.count)
      
      for(sim in 1:bootstrap_sim.count) {
        # 1. Generate a random poll result
        poll_data <- tibble(
          response = c(
            rep("success", rbinom(1, n, p)),
            rep("failure", n - rbinom(1, n, p))
          )
        )
        
        # 2. Perform bootstrap resampling
        resample_props <- numeric(bootstrap_resample.count)
        for(i in 1:bootstrap_resample.count) {
          # Take a bootstrap resample
          curr_resample <- sample(poll_data$response, size = n, replace = TRUE)
          # Compute proportion
          resample_props[i] <- mean(curr_resample == "success")
        }
        
        # 3. Calculate MOE for this simulation
        ci <- quantile(resample_props, probs = c(0.025, 0.975))
        sim_moes[sim] <- (ci[2] - ci[1]) / 2
      }
      
      # Return average MOE across all simulations
      mean(sim_moes)
    }
  ) |>
  ungroup()

# Create heatmap for bootstrap MOEs
bootstrap_heatmap <- ggplot(bootstrap.results, aes(x = p, y = n, fill = moe)) +
  geom_raster() +
  scale_fill_viridis_c(option = "cividis") +
  labs(title = "Bootstrap Margin of Error by Sample Size and Proportion",
       subtitle = "Average of multiple bootstrap simulations",
       x = "Proportion (p)",
       y = "Sample Size (n)",
       fill = "MOE") +
  theme_minimal() +
  geom_contour(aes(z = moe), breaks = c(0.02, 0.03, 0.04, 0.05, 0.06), color = "white") +
  annotate("text", x = 0.5, y = 700, 
           label = "Bootstrap MOE follows similar patterns\nas simulation and theoretical approaches", 
           color = "white", size = 3.5)


