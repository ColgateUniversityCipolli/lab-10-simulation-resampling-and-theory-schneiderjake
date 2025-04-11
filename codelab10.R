#######################################################################
#Lab 10
#Jake Schneider
#######################################################################
library(tidyverse)
library(patchwork)
library(ggplot2)
library(scales)
library(xtable)

##################################################
#Task 1
#################################################
################
#sample size 10000
poll.size <- 1004
poll.probability <- 0.39 #true prob that someone is satisfied with position of US
poll.generation <- 10000

simulation1.poll.dat <- tibble(
  prop = rbinom(poll.generation, poll.size, poll.probability) / poll.size 
)

ci1 <- quantile(simulation1.poll.dat$prop, probs = c(0.025, 0.975))
moe1 <- (ci1[2] - ci1[1]) / 2



sim.1.plot <- ggplot(simulation1.poll.dat, aes(x = prop)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#4F81BD", alpha = 0.6) +
  geom_density(color = "#C0504D", linewidth = 1.2) +
  
  # CI bounds
  geom_vline(xintercept = ci1[1], color = "darkgreen", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = ci1[2], color = "darkgreen", linetype = "dashed", linewidth = 1) +
  
  # True p
  geom_vline(xintercept = 0.39, color = "black", linetype = "solid", linewidth = 1) +
  
  # Labels and styling
  labs(
    title = "Sampling Distribution of Sample Proportion (n = 1004)",
    subtitle = "True p = 0.39 with 95% Confidence Interval bounds",
    x = expression(hat(p)),
    y = "Density"
  ) +
  scale_x_continuous(limits = c(0.34, 0.44), breaks = seq(0.3, 0.5, by = 0.02)) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )


################
#poll size is 2008
poll.size <- 2008
poll.probability <- 0.39
poll.generation <- 10000

simulation1.poll.dat <- tibble(
  prop = rbinom(poll.generation, poll.size, poll.probability) / poll.size )

ci2 <- quantile(simulation1.poll.dat$prop, probs = c(0.025, 0.975))
moe2 <- (ci2[2] - ci2[1]) / 2


# Plot
sim.2.plot <- ggplot(simulation1.poll.dat, aes(x = prop)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#4F81BD", alpha = 0.6) +
  geom_density(color = "#C0504D", linewidth = 1.2) +
  
  # CI bounds
  geom_vline(xintercept = ci2[1], color = "darkgreen", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = ci2[2], color = "darkgreen", linetype = "dashed", linewidth = 1) +
  
  # True p
  geom_vline(xintercept = 0.39, color = "black", linetype = "solid", linewidth = 1) +
  
  # Labels and styling
  labs(
    title = "Sampling Distribution of Sample Proportion (n = 2008)",
    subtitle = "True p = 0.39 with 95% Confidence Interval bounds",
    x = expression(hat(p)),
    y = "Density"
  ) +
  scale_x_continuous(limits = c(0.34, 0.44), breaks = seq(0.3, 0.5, by = 0.02)) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

basic.sim.plots <- (sim.1.plot / sim.2.plot)

ggsave("basic.sim.plots.pdf", plot = basic.sim.plots, width = 7, height = 6)

###############################
#results for the 2 basic simulations
results.table <- bind_rows(
  tibble(
    `Sample Size` = 1004,
    `Statistic` = c("True Proportion (p)", "95% CI Lower Bound", "95% CI Upper Bound", "Estimated Margin of Error"),
    `Value` = round(c(0.39, ci1[1], ci1[2], moe1), 4)
  ),
  tibble(
    `Sample Size` = 2008,
    `Statistic` = c("True Proportion (p)", "95% CI Lower Bound", "95% CI Upper Bound", "Estimated Margin of Error"),
    `Value` = round(c(0.39, ci2[1], ci2[2], moe2), 4)
  )
)

results.table <- results.table |>
  pivot_wider(names_from = `Sample Size`, values_from = Value)|>
  rename( `n=1004` = `1004`  , `n=2008` = `2008`)
  




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

R <- 5000
resamples <- tibble(p.hat = numeric(R))

for( i in 1:R){
  # Take a resample
  curr.resample <- sample(x = gallup.survery$reponse,
                          size = nrow(gallup.survery),
                          replace = T)
  # compute the stat on the resample
  resamples$p.hat[i] <- mean(curr.resample == "satisfied")
}

ci3 <- quantile(resamples$p.hat, probs = c(0.025, 0.975))
moe3 <- (ci3[2] - ci3[1]) / 2


# Create result table
results.table.resample <- tibble(
  `Statistic` = c("Resampled Proportion (p̂)", "95% CI Lower Bound", "95% CI Upper Bound", "Estimated Margin of Error"),
  `Value` = round(c(mean(resamples$p.hat), ci3[1], ci3[2], moe3), 4)
)


# Plot
resample.plot <- ggplot(resamples, aes(x = p.hat)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#4F81BD", alpha = 0.6) +
  geom_density(color = "#C0504D", linewidth = 1.2) +
  
  # CI bounds
  geom_vline(xintercept = ci3[1], color = "darkgreen", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = ci3[2], color = "darkgreen", linetype = "dashed", linewidth = 1) +
  
  # True p
  geom_vline(xintercept = 0.39, color = "black", linetype = "solid", linewidth = 1) +
  
  labs(
    title = "Bootstrap Resampling of Gallup Survey (n = 1004)",
    subtitle = "Observed p = 0.39 with 95% Confidence Interval bounds",
    x = expression(hat(p)),
    y = "Density"
  ) +
  scale_x_continuous(limits = c(0.34, 0.44), breaks = seq(0.3, 0.5, by = 0.02)) +

  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

ggsave("resample.plot.pdf", plot = resample.plot, width = 8, height = 5)


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

ggsave("moe.heatmap.pdf", plot = moe.heatmap.advanced, width = 6, height = 6)


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

ggsave("wilson.heatmap.pdf", plot = wilson.heatmap.advanced, width = 6, height = 6)


results.comparison <- tibble(
  Method = c("Simulation (n=1004)", "Simulation (n=2008)", "Bootstrap (n=1004)", "Wilson (n=1004, p=0.39)"),
  MOE = c(
    moe1,
    moe2,
    moe3,
    wilson.margin(1004, 0.39)
  )
)

moe.sim.1000 <- n.p.sim.results |>
  filter(n == 1000, p == 0.39) |>
  pull(moe)

# Wilson MOE at same values
moe.wilson.1000 <- wilson.margin(1000, 0.39)

# Create formatted comparison table
results.comparison <- tibble(
  Method = c("Simulation (n = 1000, p = 0.39)", "Wilson (n = 1000, p = 0.39)"),
  MOE = c(moe.sim.1000, moe.wilson.1000)
) |>
  mutate(MOE = round(MOE, 6))



