####################################################################################################
#######         Assessing trends and reporting biases in historic occurrences of       #############
#######                       Glacier Lake Outburst Floods                             #############
#######                                                                                #############
#######                                  by Georg Veh                                  #############
#######                               V1.0: 08 July, 2021                              #############
#######                               V1.1: 23 July, 2021                              #############
####################################################################################################


########### Data grooming

# Load the following packages, or use install.packages("nameofpackage"), if some 
# are not pre-installed in your R environment. In some cases you need to restart your R session.

# Packages for Bayesian regression models, the workhorses in this study.

require(mcp)
require(brms)

# Packages for data handling and plotting from the tidyverse / ggplot universe.

require(ggplot2)
require(tidybayes)
require(modelr)
require(tidyverse)
require(forcats)
require(ggpubr)
require(viridis)
require(scales)


# Set YOUR working directory folder where to find the R-Data object containing 
# the GLOF counts per region. Change the following lake to your location.

setwd("C:/Users/local-admin/Desktop/Plots_GLOFs_global/")

# Load the data object containing reported GLOFs globally and per region into memory.
# This object is a named list containing the annual GLOF occurrences in 7 regions (entries 1-7) 
# and globally (entry 8).

region.list <- readRDS("regional_glof_stats.rds")  


################################################################################################

### Getting an overview of GLOF occurrences in the study regions

# Plot total number of GLOFs per study region and dam type

bind_rows(region.list) %>%
  filter((region != "Global") & (region != "Other")) %>%
  filter(year >= 1901 & year < 2018) %>%
  group_by(region) %>%
  summarise(across(c(moraine, ice, other), sum)) %>%
  rowwise(region) %>%
  mutate(tot_sum = sum(c(moraine, ice, other))) %>%
  pivot_longer(cols =  c(moraine, ice, other), 
               names_to = "dam_type", 
               values_to = "Count") %>%
  ggplot(aes(x = dam_type, 
             y = Count, 
             fill = dam_type, 
             width = 1)) +
  geom_bar(stat = "identity", colour = "gray30") + 
  scale_fill_viridis_d("Dam type", option = "E") +
  facet_wrap(~ region, scales = "free") +
  scale_y_continuous(limits = c(0, 600), expand = c(0, 0)) +
  scale_x_discrete(limits = c("ice", "moraine", "other")) +
  theme_classic() +
  ylab("") + 
  theme(strip.background = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        rect = element_rect(fill = "transparent"))


# Plot total number of GLOFs per decade, study region, and dam type.

bind_rows(region.list) %>%
  filter((region != "Global") & (region != "Other")) %>%
  filter(year >= 1901 & year < 2018) %>%
  pivot_longer(cols = c(moraine, ice, other), 
               names_to = "type", 
               values_to = "count") %>%
  group_by(region, type, group = cut(year, breaks = seq(1900, 2020, 10))) %>%
  summarise(n = sum(count))  %>%
  ggplot(aes(x = group, 
             y = n, 
             fill = type, 
             width = 1)) +
  geom_bar(stat = "identity", colour = "gray30") + 
  scale_fill_viridis_d("Dam type", option = "E") +
  facet_wrap(~ region, scales = "free") +
  scale_y_continuous(limits=c(0, 110), expand = c(0, 0)) +
  theme_classic() +
  ylab("") + 
  theme(strip.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        rect = element_rect(fill = "transparent"))


# Generate Plot 1A: number of GLOFs per decade, study region, and dam type 
# in one stacked barplot.

reported_glofs_per_reg <- bind_rows(region.list) %>%
  filter((region != "Global") & (region != "Other")) %>%
  filter(year >= 1901 & year <= 2019) %>%
  mutate(freq = moraine + ice + other) %>%
  group_by(region, group = cut(year, breaks = seq(1900, 2020, 10))) %>%
  summarise(n = sum(freq)) %>%
  mutate(region = as_factor(region)) %>%
  mutate(region = fct_relevel(region, c(
    "Andes",
    "Pacific NW",
    "High Mountain Asia",
    "Iceland", 
    "Scandinavia",
    "European Alps"))) %>%
  ggplot(aes(y = n, 
             x = group,
             fill = region)) + 
  geom_bar(position = "stack",
           stat     = "identity",
           width    = 1, 
           colour   = "gray30", 
           size     = 0.1) + 
  scale_fill_viridis_d(option = "inferno", "Region") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300)) +
  theme_classic() +
  ylab("") + 
  theme(strip.background = element_blank(),
        legend.position  = "none",
        axis.title.x     = element_blank(),
        axis.text.x      = element_blank(),
        axis.ticks.x     = element_blank(),
        rect = element_rect(fill = "transparent")) 

# Save the file, here into a new folder called 'New_Plots'

if(!dir.exists("New_Plots")) {dir.create("New_Plots")}

ggsave(filename = "New_Plots/reported_glofs_per_reg.pdf",
       plot = reported_glofs_per_reg,
       width = 35, 
       height = 25, 
       units = "mm")

###### Testing the break in GLOF counts, air temperatures, and research interest ###############

# First, we test the notion of a global break in GLOF reporting that Harrison et al. (2018) located
# in the 1970s and Carrivick and Tweed (2016) in the 1990s.

# Extract the global GLOF counts

all.df <- region.list$Global %>%
  filter((year >= 1901) & (year < 2018)) %>%
  select(-year_scale) %>%
  add_column(year_scale = as.numeric(scale(.$year))) %>%
  rowwise() %>%
  mutate(freq = sum(c(moraine, ice, other))) %>%
  ungroup() %>%
  mutate(temp_scale = as.numeric(scale(temp_mean)))


# Formualte a piecewise regression model, here with two segments, using the package 'mcp'.
# To learn more about the model syntax, please see: 
# https://lindeloev.github.io/mcp/articles/formulas.html

model <- list(
  freq ~  year_scale ,  # intercept
  ~ 0  + year_scale     # model after the break point
)

# Setting priors

# We want to avoid change points in very short segments at the end of the data,
# so that we truncate the prior on the change point location to the lowest 5% and 
# highest 95% of the data.
# We set robust priors on the regression intercepts and the slopes. 
# To improve sampling efficiency, we use standardised years.

scaled.years <- sort(unique(all.df$year_scale))
t.low <- scaled.years[round(0.05*length(scaled.years))]
t.up  <- scaled.years[round(0.95*length(scaled.years))]

priors = list(
  int_1           = "dt( 2, 3, 3)",  # Intercept of the model
  year_scale_1    = "dt( 0, 2, 3)",  # Slope of the first segment
  year_scale_2    = "dt( 0, 2, 3)",  # Slope of the second segment
  cp_1            = paste0("dt(0, 1, 3) T(", t.low, " , ", t.up, ")") # Location of the breakpoint
)

# Run the piecewise regression model. We choose a Poisson likelihood, which
# is the default distribution for count data.

fit.freq <- mcp(model  = model, 
                data   = all.df, 
                prior  = priors, 
                family = poisson(link = "log"), 
                chains = 4, 
                iter   = 20000, 
                cores  = 4, 
                adapt  = 8000)

# Show the posterior distribution of the model parameters and 
# sampling statistics.

summary(fit.freq)

# Calculate the log predictive density (ELPD) that summarises the predictive error of a Bayesian model.
# This is a useful measure assess the error of the model and compare models with each other.

loo1 <- loo(fit.freq)

# To generate a plot showing both the prior and posterior location of the
# change point, we need convert the standardised data back to the original scale.
# This is a bit of data wrangling in the next few lines.

prior.cp1.x.freq <- seq(from = t.low, to = t.up, length.out = 150)

prior.xy.cp1.freq <- dnorm(x = prior.cp1.x.freq , mean = 0, sd = 3)

prior.x.cp1.orig.freq <- (prior.cp1.x.freq * sd(all.df$year)) + mean(all.df$year)

prior.line <- tibble(x = prior.x.cp1.orig.freq,
                     y = rescale(x = prior.xy.cp1.freq, 
                                 to = c(0, 0.05* max(all.df$freq))))

post_cp_freq <- do.call(rbind, fit.freq$mcmc_post)

post.x.orig.freq <- (post_cp_freq[, "cp_1"] * sd(all.df$year)) + mean(all.df$year)

density.post.orig.freq <- density(post.x.orig.freq)
density.post.rescaled.freq <- rescale(x = density.post.orig.freq$y, 
                                      to = c(0, 0.05* max(all.df$freq)))

post.cp.polygon <- tibble(x = density.post.orig.freq$x,
                          y = rescale(x = density.post.orig.freq$y, 
                                      to = c(0, 0.05* max(all.df$freq))))

q.glof <- unname(round(quantile(post.x.orig.freq, c(0.05, 0.5, 0.95))))
q.glof.up <- paste0("+", q.glof[3] - q.glof[2])
q.glof.low <- paste0("-", q.glof[2] - q.glof[1])

# Obtain the posterior predictive distribution.

predict.freq <- predict(fit.freq, 
                        summary = F, 
                        nsamples = 2000, 
                        samples_format = "tidy")

# Generate a plot (Fig. 1B), showing the 
# - total historic GLOF count,
# - posterior predictive distribution from the piecwise model,
# - the prior and posterior distribution of the break point location.

plot_glofs <- predict.freq %>%
  mutate(year = year_scale * sd(all.df$year) + mean(all.df$year)) %>%
  ggplot(aes(x = year, y = predict)) +
  stat_lineribbon(aes(y = predict), 
                  point_interval = mean_qi, 
                  alpha = 0.6) +
  scale_fill_brewer(name = "Posterior\npredictive",
                    palette = "Blues") +
  geom_point(data   = all.df,
             aes(x  = year, y = freq),
             size   = 1.5, 
             shape  = 21, 
             fill   = "black", 
             color  = "white", 
             stroke = 0.5) +
  theme_bw() +
  labs(x = "Year",
       y = "Number of GLOFs") +
  geom_line(data = prior.line, aes(x = x, y = y)) +
  geom_polygon(data = post.cp.polygon, aes(x = x, y = y)) +
  annotate("text", 
           label =  substitute(paste("CP: "*  x^y * "/"[z]*" yrs"), 
                               list(x = q.glof[2],
                                    y = q.glof.up, 
                                    z = q.glof.low)),
           x = 1900, y = 37, hjust = 0, size = 3) +
  theme(legend.position = 'none')


# We compare the model with one breakpoint in the annual trend  
# against the simpler model of no change point.

model <- list(
  freq ~  year_scale # Only with intercept and slope, no breakpoint
)

# Setting priors.

priors <- list(
  int_1           = "dt( 2, 3, 3)", # Intercept
  year_scale_1    = "dt( 0, 2, 3)"  # slope
)

# Running the model without a breakpoint.

fit.freq.no.bp <- mcp(model =  model,
                      data =   all.df,
                      prior =  priors,
                      family = poisson(link = "log"), 
                      chains = 4, 
                      iter  =  20000, 
                      cores =  4, 
                      adapt =  4000)

# Print the model summary.

summary(fit.freq.no.bp)

# Assess ELPD for this model.

loo2 <- loo(fit.freq.no.bp)

# Compare both models to assess which one has the lower predictive error.

m1 <- data.frame(print(loo_compare(loo1, loo2), simplify = F))
m1 <- round(m1, digits = 1)
rownames(m1)[rownames(m1) == "model1"] <- "GLOF Counts: 1 BP"
rownames(m1)[rownames(m1) == "model2"] <- "GLOF Counts: No BP"

################################################################################################

# Second, we test whether we find breakpoints also in the number of annual glacier surveys.

model2 <- list(
  mb_and_front ~  year_scale ,  # first segment
  ~ 0  + year_scale             # second segment
)

# Setting priors: same priors on the change point location, robust priors on the intercept 
# and the slopes.

scaled.years <- sort(unique(all.df$year_scale))
t.low <- scaled.years[round(0.05*length(scaled.years))]
t.up  <- scaled.years[round(0.95*length(scaled.years))]

priors2 <- list(
  int_1           = "dt( 6, 6, 3)",  # Intercept
  year_scale_1    = "dt( 0, 2, 3)",  # slope
  year_scale_2    = "dt( 0, 2, 3)",  # slope
  cp_1            = paste0("dt(0, 1, 3) T(", t.low, " , ", t.up, ")") # Truncated priors on the BP
)

# Run the piecewise Bayesian model of GLOF counts versus the annual number of glacier surveys.

fit.mb_and_front <- mcp(model  = model2, 
                        data   = all.df, 
                        prior  = priors2, 
                        family = poisson(link = "log"), 
                        chains = 4, 
                        iter   = 20000, 
                        cores  = 4, 
                        adapt  = 4000)

# Summarise the model fit.

summary(fit.mb_and_front)

# Calculate the ELPD.

loo3 <- loo(fit.mb_and_front)

# Generate graphical elements for the prior and posterior location of the break point.

prior.cp1.x.mb_and_front <- seq(from = t.low, to = t.up, length.out = 150)

prior.xy.cp1.mb_and_front <-  dnorm(x = prior.cp1.x.mb_and_front, 
                                    mean = 0, 
                                    sd = 3)

prior.x.cp1.orig.mb_and_front <- (prior.cp1.x.mb_and_front * sd(all.df$year)) + mean(all.df$year)

prior.line <- tibble(x = prior.x.cp1.orig.mb_and_front,
                     y = rescale(x = prior.xy.cp1.mb_and_front, 
                                 to = c(0, 0.05* max(all.df$mb_and_front))))

post_cp_mb_and_front <- do.call(rbind, fit.mb_and_front$mcmc_post)

post.x.orig.mb_and_front <- (post_cp_mb_and_front[, "cp_1"] * sd(all.df$year)) + mean(all.df$year)

density.post.orig.mb_and_front <- density(post.x.orig.mb_and_front)

density.post.rescaled.mb_and_front <- rescale(x = density.post.orig.mb_and_front$y, 
                                              to = c(0, 0.05* max(all.df$mb_and_front)))

post.cp.polygon <- tibble(x = density.post.orig.mb_and_front$x,
                          y = rescale(x = density.post.orig.mb_and_front$y, 
                                      to = c(0, 0.05* max(all.df$mb_and_front))))

q.glof <- unname(round(quantile(post.x.orig.mb_and_front, c(0.05, 0.5, 0.95))))
q.glof.up <- paste0("+", q.glof[3] - q.glof[2])
q.glof.low <- paste0("-", q.glof[2] - q.glof[1])

# Generate the posterior predictive distribution of glacier surveys for each year 
# in the study period.

predict.mb_and_front <- predict(fit.mb_and_front, 
                                summary = F, 
                                nsamples = 2000, 
                                samples_format = "tidy")


# Generate a plot (Fig. 1D), showing the 
# - total annual number of glacier surveys,
# - posterior predictive distribution from the piecwise model,
# - the prior and posterior distribution of the break point location.

plot_glac <- predict.mb_and_front %>%
  mutate(year = year_scale * sd(all.df$year) + mean(all.df$year)) %>%
  ggplot(aes(x = year, y = predict)) +
  stat_lineribbon(aes(y = predict), 
                  point_interval = mean_qi, 
                  alpha = 0.6) +
  scale_fill_brewer(name = "Posterior\npredictive",
                    palette = "Purples") +
  geom_point(data = all.df,
             aes(x = year, y = mb_and_front),
             size = 1.5, 
             shape = 21, 
             fill = "black", 
             color = "white",
             stroke = 0.5) +
  theme_bw() +
  labs(x = "Year",
       y = "Number of\nsurveyed glaciers") +
  geom_line(data = prior.line, aes(x = x, y = y)) +
  geom_polygon(data = post.cp.polygon, aes(x = x, y = y)) +
  annotate("text", label =  substitute(paste("CP: "*  x^y * "/"[z]*" yrs"), 
                                       list(x = q.glof[2], 
                                            y = q.glof.up, 
                                            z = q.glof.low)),
           x = 1900, 
           y = 800, 
           hjust = 0, 
           size = 3) +
  theme(legend.position = 'none')


# Again, we compare the model that assumes a change point in glacier surveys
# against a simpler model that has no change point.

model2 <- list(
  mb_and_front ~  year_scale  #  Only intercept and slope
)

# Setting priors, same as for the model with one breakpoint.

priors2 <- list(
  int_1           = "dt( 6, 6, 3)",  # Intercept
  year_scale_1    = "dt( 0, 2, 3)"  # slope
  
)

# Run the model of glacier surveys versus time without a breakpoint.

fit.mb_and_front.nobp = mcp(model  = model2, 
                            data   = all.df, 
                            prior  = priors2, 
                            family = poisson(link = "log"), 
                            chains = 4, 
                            iter   = 20000, 
                            cores  = 4, 
                            adapt  = 4000)

# Calculate ELPD and compare both models with and without a changepoint.

loo4 <- loo(fit.mb_and_front.nobp)

m2 <- data.frame(print(loo_compare(loo3, loo4), simplify = F))
m2 <- round(m2, digits = 1)
rownames(m2)[rownames(m2) == "model1"] <- "Glac_meas: 1 BP"
rownames(m2)[rownames(m2) == "model2"] <- "Glac_meas: No BP"

# Assess changes in glaciological studies before and after the change point in 1973.

all.df.sub <- all.df[match(c(1901, 1973, 2017), all.df$year), ]

pred.change.mb_front <- predict(fit.mb_and_front, all.df.sub, 
                                probs = c(0.025, 0.5, 0.975))

# Obtain the average increase in the posterior annual number of glacier surveys between 1901 and 1973,
# and between 1973 and 2017.

(pred.change.mb_front$Q50[2] - pred.change.mb_front$Q50[1]) / (all.df.sub$year[2] - all.df.sub$year[1])
(pred.change.mb_front$Q50[3] - pred.change.mb_front$Q50[2]) / (all.df.sub$year[3] - all.df.sub$year[2])

################################################################################################

# Third, and finally, we test whether we find a breakpoint in annual air temperatures.

model3 <- list(
  temp_scale ~  year_scale ,  # intercept and slope in the first segment
  ~ 0  + year_scale           # second segment
)

# Setting priors.
# Compared to count data, air temperatures are neither integer, nor strictly positive.
# We thus choose a Gaussian likelihood, and need to define and additional noise term 'sigma'. 

scaled.years <- sort(unique(all.df$year_scale))
t.low <- scaled.years[round(0.05*length(scaled.years))]
t.up  <- scaled.years[round(0.95*length(scaled.years))]

priors3 = list(
  int_1           = "dt( 0, 2, 3)",  # Intercept
  year_scale_1    = "dt( 0, 2, 3)",  # Slope 1
  year_scale_2    = "dt( 0, 2, 3)",  # Slope 2
  cp_1            = paste0("dt(0, 1,3) T(", t.low, " , ", t.up, ")"), # Change point
  sigma_1 = "dt( 0, 2, 3) T(0, )"    # Noise
)

# Fit the piecewise model of mean annual air temperatures vs. time.

fit.temp <- mcp(model = model3, 
                data = all.df, 
                prior = priors3, 
                family = gaussian(), 
                chains = 4, 
                iter =   20000, 
                cores =  4, 
                adapt =  4000)

# Assess model fit.

sum.fit.temp <- summary(fit.temp)

# Calculate warming trends before and after the change point on the real (unstandardised) scale.

sum.fit.temp[(sum.fit.temp$name == "year_scale_1") | (sum.fit.temp$name ==  "year_scale_2"), 2:4 ] * 
  sd(all.df$temp_mean) / sd(all.df$year)

# Calculate the ELPD.

loo5 <- loo(fit.temp)

# As above, generate the plot of Annual air temperatures vs. time.

prior.cp1.x.temp <- seq(from = t.low, to = t.up, length.out = 150)

prior.xy.cp1.temp <- dnorm(x = prior.cp1.x.temp, mean = 0, sd = 3)

prior.x.cp1.orig.temp <- (prior.cp1.x.temp * sd(all.df$year)) + mean(all.df$year)

prior.line <- tibble(x = prior.x.cp1.orig.temp,
                     y = rescale(x = prior.xy.cp1.temp, 
                                 to = c(0, 0.05* max(all.df$temp_mean))))

post_cp_temp <- do.call(rbind, fit.temp$mcmc_post)

post.x.orig.temp <- (post_cp_temp[, "cp_1"] * sd(all.df$year)) + mean(all.df$year)

density.post.orig.temp <- density(post.x.orig.temp)
post.cp.polygon <- tibble(x = density.post.orig.temp$x,
                          y = rescale(x = density.post.orig.temp$y, 
                                      to = c(0, 0.05* max(all.df$temp_mean))))

q.glof <- unname(round(quantile(post.x.orig.temp, c(0.05, 0.5, 0.95))))
q.glof.up <- paste0("+", q.glof[3] - q.glof[2])
q.glof.low <- paste0("-", q.glof[2] - q.glof[1])


# Generate the posterior predictive distribution of air temperatures for each year 
# in the study period.

predict.temp <- predict(fit.temp, 
                        summary = F, 
                        nsamples = 2000, 
                        samples_format = "tidy")

# Plot Figure 1C.

plot_temp <- predict.temp %>%
  mutate(year = year_scale * sd(all.df$year) + mean(all.df$year),
         predict = predict * sd(all.df$temp_mean) + mean(all.df$temp_mean)) %>%
  ggplot(aes(x = year, y = predict)) +
  stat_lineribbon(aes(y = predict), 
                  point_interval = mean_qi, alpha = 0.6) +
  scale_fill_brewer(name = "Posterior\npredictive",
                    palette = "Oranges") +
  geom_point(data = all.df,
             aes(x = year, y = temp_mean),
             size = 1.5, shape = 21, fill = "black", color = "white", stroke = 0.5) +
  theme_bw() +
  labs(x = "Year",
       y = "Mean annual air\ntemperature [°C]") +
  geom_line(data = prior.line, aes(x = x, y = y)) +
  geom_polygon(data = post.cp.polygon, aes(x = x, y = y)) +
  annotate("text", label =  substitute(paste("CP: "*  x^y * "/"[z]*" yrs"), 
                                       list(x = q.glof[2], 
                                            y = q.glof.up, 
                                            z = q.glof.low)),
           x = 1900, 
           y = 2.25,  
           hjust = 0, 
           size = 3) +
  theme(legend.position = 'none')


# Finally, we compare the temperature model with one breakpoint against the simpler
# model that has no change point.

model3 <- list(
  temp_scale ~  year_scale 
)

# Setting priors

priors3 <- list(
  int_1           = "dt( 0, 2, 3)",  # Intercept
  year_scale_1    = "dt( 0, 2, 3)"
)

# And running the model.

fit.temp.nobp <- mcp(model  = model3, 
                     data   = all.df, 
                     prior  = priors3, 
                     family = gaussian(), 
                     chains = 4, 
                     iter   = 20000, 
                     cores  = 4, 
                     adapt  = 4000)

# Assess model fit.

summary(fit.temp.nobp)

# Estimate ELPD.

loo6 <- loo(fit.temp.nobp)

# Finally, compare the model with and without an assumed break in temperature.

m3 <- data.frame(print(loo_compare(loo5, loo6), simplify = F))
m3 <- round(m3, digits = 1)
rownames(m3)[rownames(m3) == "model1"] <- "Temperature: 1 BP"
rownames(m3)[rownames(m3) == "model2"] <- "Temperature: No BP"

# Write a table of all LOOs to disk. Change the filename, if necessary.

write.table(rbind(m1, m3, m2), 
            file = "Model_performance.csv", 
            dec = ".",
            sep = "\t",
            row.names = T,
            quote = F)

############### Plot Fig. 1: GLOFs, temperatures, and glacier studies with BP #################

p_glof_temp_glac <- ggarrange(
  plotlist = list(plot_glofs, plot_temp, plot_glac),
  ncol = 1,
  nrow = 3,
  font.label = list(size = 10),
  legend = "bottom",
  labels = c("b", "c", "d"),
  label.x = 0,
  label.y = 1,
  hjust = -0.5,
  vjust = 1.5,
  align = "hv",
  common.legend = T
)

# Save to disk. Change filename and location, if necessary.

ggsave(filename = "New_Plots/p_glof_temp_glac2.pdf",
       plot =  p_glof_temp_glac,
       width = 75, height = 190, units = "mm")

################################################################################################

# Plot Fig. S1, the posterior slopes before and after the change point from all three models. 
# The parameter values here refer to standardised input data.

plot_post_slopes <- bind_rows(
  post_cp_freq %>% 
    as_tibble() %>% 
    mutate(Model = "GLOF counts"),
  post_cp_temp %>% 
    as_tibble() %>% 
    mutate(Model = "Temperature"), 
  post_cp_mb_and_front %>% 
    as_tibble() %>% 
    mutate(Model = "Glac. surveys")) %>%
  rename(b1 = year_scale_1,
         b2 = year_scale_2) %>% 
  pivot_longer(cols= c(b1, b2), 
               names_to = "slopes",
               values_to = "post_vals") %>%
  mutate(Model = as_factor(Model) %>% 
           fct_relevel("Glac. surveys", 
                       "Temperature", 
                       "GLOF counts")) %>%
  ggplot(aes(x = post_vals,
             y = Model,
             fill = slopes)) +
  coord_cartesian(xlim = c(-0.2, 3)) + 
  stat_halfeye(shape = 21,
               point_color = "navy",
               point_fill = "white",
               point_size = 2.5,
               slab_alpha = 0.75,
               slab_color = "gray25",
               interval_size = 1.5 ,
               interval_color = "navy") + 
  scale_fill_manual(values =  c("navy", "darkorange"),
                    name = "Parameter",
                    labels = c(expression(beta[1]* " (bef. CP)"),
                               expression(beta[2]* " (aft. CP)"))) + 
  labs(x = "Standardised\nregression slopes",
       y = "Model") +
  geom_vline(xintercept = 0) + 
  theme_bw()

# Save the plot to disk. Change the location & filename, if necessary. 

ggsave(plot = plot_post_slopes,  
       filename = "New_Plots/plot_post_slopes_three_models.png",
       width = 130,
       height = 75,
       units = "mm")


##############################################################################################################
### Trends in GLOF activity since AD 1973
##############################################################################################################

# Now wish to learn more about the trends in GLOF activity after 1973, which is when all
# change point intervals overlap. We distinguish between regions and dam types.

# Generate a training set with data for the period 1973-2017. 
# We standardise the input data year, temperature, and research activity to zero mean and 
# unit standard deviation.

dat <- bind_rows(region.list) %>%
  filter((region != "Global") & (region != "Other")) %>%
  filter(year >= 1973 & year < 2018) %>%
  pivot_longer(cols = c(moraine, ice, other), names_to = "type", values_to = "count") %>%
  mutate(year_scale = as.numeric(scale(year)),
         temp_scale = as.numeric(scale(temp_mean)),
         meas_scale = as.numeric(scale(log10(mb_and_front+1))))

# Generate a test set with data before the overlapping breakpoint in 1973
# Note that we need to use the mean and standard deviation from the preceding period
# to standardise the data.

dat2 <- bind_rows(region.list) %>%
  filter((region != "Global") & (region != "Other")) %>%
  filter(year >= 1901 & year < 1973) %>%
  pivot_longer(cols = c(moraine, ice, other), names_to = "type", values_to = "count") %>%
  mutate(meas_scale = (log10(mb_and_front+1) - mean(log10(dat$mb_and_front+1))) / sd(log10(dat$mb_and_front+1)),
         temp_scale = (temp_mean - mean(dat$temp_mean)) / sd(dat$temp_mean),
         year_scale = (year - mean(dat$year)) / sd(dat$year)) 


# Run a Bayesian regression model with Poisson likelihood. We 
# account for zero-inflated counts in annual GLOF counts. The model
# also allows for random effects by dam type and region, such that we 
# set up a hierarchical model, in which the regions are nested within 
# the three dam types. 

# First set robust priors on the intercepts and slopes, and normal priors 
# on the standard deviations. Maintain default priors on all other parameters.

bprior <- prior(student_t(3, 0, 2.5), class = "Intercept") + 
          prior(student_t(3, 0, 2.5), class = "b") + 
          prior(normal(0, 2.5), class = "sd") +
          prior(normal(5, 3), class = "sd", dpar = "zi") 


# Run the hierarchical model to estimate trends in GLOF reporting in the period 1973-2018.

fit.trend.after.1973  <- brm(
                  bf(count ~ year_scale +  ( year_scale | type:region),
                     zi ~ 1 + (1 | type:region)),
                  family = zero_inflated_poisson(),
                  data   = dat,
                  prior  = bprior,
                  cores  = 4,
                  chains = 4,
                  warmup = 1500,
                  iter   = 3500,
                  control = list(adapt_delta = 0.95, 
                                 max_treedepth = 12))

# Assess model parameters, check for divergences, and do posterior predictive checks.

summary(fit.trend.after.1973)
plot(fit.trend.after.1973)
pp_check(fit.trend.after.1973)

# Extract the posterior GLOF rate for each dam type and region.
# Define the range of years for which we want to obtain posterior draws.

conds <- dat %>% 
  group_by(region) %>% 
  summarise(min_r = min(year_scale), 
            max_r = max(year_scale))

# Obtain the posterior distribution of GLOF rates for each region and dam type.

preds <- dat %>%
  data_grid(year_scale = seq_range(year_scale, n = 101),
            type       = unique(dat$type),
            region     = unique(dat$region)) %>%
  add_fitted_draws(model = fit.trend.after.1973, 
                   value = "count", 
                   n = 1000)

preds.sub <- list()

for (i in 1:nrow(conds)) {
  
  preds.sub[[i]] <- filter(preds, 
                           region == conds$region[i] & 
                             year_scale >= conds$min_r[i] &  
                             year_scale <= conds$max_r[i])
}

preds.sub <- bind_rows(preds.sub)

# Plot the posterior rates in GLOF reporting for each region and dam type.

plot_trend_year <- preds.sub %>%
  mutate(year = year_scale * sd(dat$year) + mean(dat$year)) %>%
  ggplot(aes(x = year, y = count, color = type)) +
  facet_wrap(~region, scales = "free", ncol = 2) +
  geom_point(data = dat,
             aes(x = year, y = count, color = type),
             shape = 16) +
  scale_color_manual(name = "Dam type", values = cividis(3)) +
  scale_fill_manual(name = "Posterior rate", values = "#52c8c8c8") +
  stat_lineribbon(aes(y = count), .width = 0.95,
                  point_interval = mean_qi) +
  theme_bw() +
  labs(x = "Year",
       y = "Number of GLOFs")


# Obtain the posterior 95% highest density interval (HDI) for the trends in
# GLOF reporting for all regions and dam types. Note that these refer
# to the standardised predictor year.

fit.trend.after.1973 %>%
  spread_draws(b_year_scale, `r_type:region`[region, param]) %>%
  filter(param == "year_scale") %>%
  mutate( region_mean = b_year_scale + `r_type:region`) %>% 
  group_by(region) %>% 
  summarise(quantslow = quantile(region_mean, 0.025), 
            quantsup = quantile(region_mean, 0.975))

# The same statistic as a plot.

mod_param <- fit.trend.after.1973 %>%
  spread_draws(b_year_scale, `r_type:region`[region, param]) %>%
  filter(param == "year_scale") %>%
  mutate( region_mean = b_year_scale + `r_type:region`) %>%
  ungroup() %>%
  mutate(region = str_replace_all(region, "[.]", "\n")) %>%
  separate(col = region, into = c("type", "region2"), sep ="_") %>%
  mutate(region2 = as_factor(region2) %>% fct_rev(),
         type = as_factor(type) %>% fct_rev()) %>%
  ggplot(aes(x = region_mean,
             y = region2,
             fill = type)) +
  coord_cartesian(xlim = c(-2, 2)) + 
  stat_halfeye(shape = 21,
               point_size = 1.5,
               slab_alpha = 0.5,
               slab_color = "gray25", slab_size = 0.5,
               interval_size = 2,
               interval_color = "black",
               position = position_dodge(width = .6)) + 
  scale_fill_viridis_d(option = "E",
                       direction = -1,
                       name = "Dam type") + 
  theme_bw() +
  labs(x = "Standardised\nregression slope",
       y = "Region") +
  geom_vline(xintercept = 0)

# Merge the rates in GLOF reporting and the posterior
# distributions of the trends into one plot to generate Fig. 2.

p_1973 <- ggarrange(
  plotlist = list(plot_trend_year, mod_param),
  ncol = 2,
  font.label = list(size = 10),
  legend = "bottom",
  labels = "auto",
  label.x = 0,
  widths  = c(2,1),
  label.y = 1,
  hjust = -0.5,
  vjust = 1.5,
  align = "h",
  common.legend = T
)

# Save this figure to disk. We manually changed the colors to grey shades for trends
# that were not credibly different from zero.

ggsave(filename = "New_Plots/glof_trends_since_1973.svg",
       plot =  p_1973,
       width = 180, 
       height = 120, 
       units = "mm")

##############################################################################################################
### GLOFs versus mean annual air temperatures and research activity since AD 1973
##############################################################################################################

# Now wish to learn more about the drivers in GLOF reporting in this period. 
# We therefore predict GLOF counts for a given region and dam type from
# mean annual air temperatures and the number of glacier surveys, assuming 
# that these predictors stand for physical and research-driven trends in GLOF reporting.

# We set robust priors on the intercepts and slopes, and normal priors 
# on the standard deviations. Maintain default priors on all other parameters. 

bprior <- prior(student_t(3, 0, 2.5), class = "Intercept") + 
  prior(student_t(3, 0, 2.5), class = "b", coef = "meas_scale") + 
  prior(student_t(3, 0, 2.5), class = "b", coef = "temp_scale") + 
  prior(normal(0, 2.5), class = "sd") +
  prior(normal(5, 3), class = "sd", dpar = "zi") 

# Run the hierarchical Poisson regression model to estimate GLOF counts
# from mean annual air temperatures and the annual number of surveyed glaciers.

fit.temp.meas.after.1973 <- brm(bf(count ~ temp_scale + meas_scale + 
                                     ( temp_scale + meas_scale | type:region), 
                                   zi ~ 1 + (1 | type:region)),
                                   family = zero_inflated_poisson(),
                                   data   = dat,
                                   prior  = bprior,
                                   cores  = 4,
                                   chains = 4,
                                   warmup = 1500,
                                   iter   = 3500 ,
                                   control = list(adapt_delta = 0.97,
                                        max_treedepth = 12))

# Summarise the model fit.

summary(fit.temp.meas.after.1973)
plot(fit.temp.meas.after.1973)
pp_check(fit.temp.meas.after.1973)

# Extract the posterior GLOF rate for a given dam type and region for a given temperature.
# Define the range of air temperatures for which we want to obtain posterior draws.
# We curtail the data range to the observed range of temperatures in a given region.

conds <- dat %>% 
  group_by(region) %>% 
  summarise(min_r = min(temp_scale), 
            max_r = max(temp_scale))

# Obtain the posterior distribution. Keep the number of glaciological studies
# fixed to show only the effects of temperature.

preds <- dat %>%
  data_grid(temp_scale = seq_range(temp_scale, n = 101),
            meas_scale = 0,
            type       = unique(dat$type),
            region     = unique(dat$region)) %>%
  add_fitted_draws(model = fit.temp.meas.after.1973 , 
                   value = "count", 
                   n = 2000)

preds.sub <- list()

for (i in 1:nrow(conds)) {
  
  preds.sub[[i]] <- filter(preds, 
                           region == conds$region[i] & 
                             temp_scale >= conds$min_r[i] &  
                             temp_scale <= conds$max_r[i])
}

preds.sub <- bind_rows(preds.sub)

# Generate a plot, showing the trends in GLOF reporting with annual air temperatures.

plot_trend_temp <- preds.sub %>%
  mutate(temp = (temp_scale * sd(dat$temp_mean)) + mean(dat$temp_mean)) %>%
  ggplot(aes(x = temp, 
             y = count, 
             color = type)) +
  facet_wrap(~region, 
             scales = "free",
             ncol = 2) +
  scale_color_manual(name = "Dam type", values = cividis(3)) +
  scale_fill_manual(name = "Posterior rate", values = "#E6C8C8C8") +
  stat_lineribbon(aes(y = count), 
                  .width = 0.95,
                  point_interval = mean_qi) +
  geom_point(data = dat,
             aes(x = temp_mean, 
                 y = count, color = type),
             shape = 16) +
  theme_bw() +
  labs(x = "Mean annual air temperature [°C]",
       y = "GLOF reporting rate")


# Obtain the posterior 95% highest density interval (HDI) for the trends in
# glacier surveys and for annual air temperatures for all regions and dam types. 
# Note that these refer to the standardised predictor year.

fit.temp.meas.after.1973  %>%
  spread_draws(b_meas_scale, `r_type:region`[region, param]) %>%
  filter(param == "meas_scale") %>%
  mutate( region_mean = b_meas_scale + `r_type:region`) %>%
  summarise(quantslow = quantile(region_mean, 0.025), 
            quantsup = quantile(region_mean, 0.975))

fit.temp.meas.after.1973  %>%
  spread_draws(b_temp_scale, `r_type:region`[region, param]) %>%
  filter(param == "temp_scale") %>%
  mutate( region_mean = b_temp_scale + `r_type:region`) %>%
  summarise(quantslow = quantile(region_mean, 0.025), 
            quantsup = quantile(region_mean, 0.975))

# Generate a plot for both posterior distributions.

mod_param_meas <- fit.temp.meas.after.1973  %>%
  spread_draws(b_meas_scale, `r_type:region`[region, param]) %>%
  filter(param == "meas_scale") %>%
  mutate( region_mean = b_meas_scale + `r_type:region`) %>%
  ungroup() %>%
  mutate(region = str_replace_all(region, "[.]", "\n")) %>%
  separate(col = region, into = c("type", "region2"), sep ="_") %>%
  mutate(region2 = as_factor(region2) %>% fct_rev(),
         type = as_factor(type) %>% fct_rev()) %>%
  ggplot(aes(x = region_mean,
             y = region2,
             fill = type)) +
  coord_cartesian(xlim = c(-2, 2)) + 
  stat_halfeye(shape = 21,
               point_size = 3,
               slab_alpha = 0.5,
               slab_color = "gray25",
               interval_size = 2,
               interval_color = "black",
               position = position_dodge(width = .6)) + 
  scale_fill_viridis_d(option = "E",
                       direction = -1,
                       name = "Dam type") + 
  theme_bw() +
  labs(x = "Standardised\nregression slopes\nGlac. surveys",
       y = "") +
  geom_vline(xintercept = 0)

mod_param_temp <- fit.temp.meas.after.1973  %>%
  spread_draws(b_temp_scale, `r_type:region`[region, param]) %>%
  filter(param == "temp_scale") %>%
  mutate( region_mean = b_temp_scale + `r_type:region`) %>%
  ungroup() %>%
  mutate(region = str_replace_all(region, "[.]", "\n")) %>%
  separate(col = region, into = c("type", "region2"), sep ="_") %>%
  mutate(region2 = as_factor(region2) %>% fct_rev(),
         type = as_factor(type) %>% fct_rev()) %>%
  ggplot(aes(x = region_mean,
             y = region2,
             fill = type)) +
  coord_cartesian(xlim = c(-2, 2)) + 
  stat_halfeye(shape = 21,
               point_size = 3,
               slab_alpha = 0.5,
               slab_color = "gray25",
               interval_size = 2,
               interval_color = "black",
               position = position_dodge(width = .6)) + 
  scale_fill_viridis_d(option = "E",
                       direction = -1,
                       name = "Dam type") + 
  theme_bw() +
  labs(x = "Standardised\nregression slopes\nAir temperatures",
       y = "") +
  geom_vline(xintercept = 0)

# Merge the plots with the trends in GLOF rates, and the posterior
# distributions of the parameters weights on annual air temperature and
# glacier surveys to generate Fig. 3.

p_temps <- ggarrange(
  plotlist = list(plot_trend_temp, mod_param_temp,  mod_param_meas),
  ncol = 3,
  widths = c(2, 1,1),
  font.label = list(size = 10),
  legend = "bottom",
  labels = "auto",
  label.x = 0,
  label.y = 1,
  hjust = -0.5,
  vjust = 1.5,
  align = "v",
  common.legend = T
)

# Save the plot to disk

ggsave(filename = "New_Plots/glof_trends_temp_meas_since_1973.pdf",
       plot =  p_temps,
       width = 240, 
       height = 150, 
       units = "mm")


##############################################################################################################
### Hindcasting GLOFs from trends in GLOF reporting, and 
### mean annual air temperatures and research activity since AD 1973.
##############################################################################################################

# Summarise reported GLOF counts in the period before 1973.

true_counts_hindcast <- dat2 %>% 
  group_by(region, type) %>% 
  summarise(value = sum(count)) 

all_counts_hindcast <- true_counts_hindcast %>% 
  group_by(region) %>% 
  summarise(value = sum(value)) %>% 
  mutate(type = "all") %>%
  bind_rows(true_counts_hindcast) %>%
  mutate(region = ifelse(region == "High Mountain Asia", 
                         "HMA", region))

# Hindcast 1: Predict GLOFs for each year before 1973 from the observed temperature and number of 
# glacier surveys.

pred.test <- brms:::predict.brmsfit(fit.temp.meas.after.1973 , 
                                    newdata = dat2, 
                                    summary = F)
pred.test <- t(pred.test)  
tt <- bind_cols(Region = dat2$region, 
                type = dat2$type,
                year = dat2$year, 
                as_tibble(pred.test))


# Summarise the predictions by region.

sim_by_reg <- list()

for(i in unique(tt$Region)) {
  
  all <- tt %>% 
    filter(Region == i) %>%
    select(starts_with("V")) %>%
    colSums() %>% 
    as_tibble() %>%
    mutate(type = "all",
           region = i)
  
  for(j in unique(tt$type)) {
    
    reg <- tt %>%
      filter(Region == i & type == j) %>%
      select(starts_with("V")) %>%
      colSums()  %>% 
      as_tibble() %>%
      mutate(type = j ,
             region = i)
    
    all <- bind_rows(all, reg)
    
  }
  
  sim_by_reg[[i]] <- all
  
}

sims <- bind_rows(sim_by_reg) %>%
  mutate(model = "Temp.\n+ Glac. surv.")  %>%
  group_by(region, type) %>%
  filter( (value <= quantile(value, 0.975))) %>%
  ungroup()


##################################################################

# Hindcast 2: Predict the total number of GLOFs for each year before 1973 from 
# the GLOF rate in the subsequent period.

# Code remains the same.

pred.test.2 <- brms:::predict.brmsfit(fit.trend.after.1973, 
                                      newdata = dat2, 
                                      summary = F)

pred.test.2 <- t(pred.test.2)  
tt.2 <- bind_cols(Region = dat2$region, 
                  type =   dat2$type,
                  year =   dat2$year, 
                  as_tibble(pred.test.2))


sim_by_reg.2 <- list()

for(i in unique(tt.2$Region)) {
  
  all <- tt.2 %>% 
    filter(Region == i) %>%
    select(starts_with("V")) %>%
    colSums() %>% 
    as_tibble() %>%
    mutate(type = "all",
           region = i)
  
  for(j in unique(tt.2$type)) {
    
    reg <- tt.2 %>%
      filter(Region == i & type == j) %>%
      select(starts_with("V")) %>%
      colSums()  %>% 
      as_tibble() %>%
      mutate(type   = j,
             region = i)
    
    all <- bind_rows(all, reg)
    
  }
  
  sim_by_reg.2[[i]] <- all
  
}

sims.2 <- bind_rows(sim_by_reg.2) %>% 
  mutate(model = "Year") %>%
  group_by(region, type) %>%
  filter((value <= quantile(value, 0.975))) %>%
  ungroup()

# Generate a violin plot that shows the posterior predictive distribution
# of GLOF counts for each region, summed over the entire period 1901-1972.

hindcast_temp_glsurv_and_year  <- bind_rows(sims, sims.2) %>% 
  filter(type == "all") %>%
  mutate(region = ifelse(region == "High Mountain Asia", "HMA", region)) %>%
  ggplot(aes(model, value, fill = model)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
              scale = "width", 
              color = "black") +
  scale_fill_manual(values = c("lightblue", "deepskyblue3" ),
                    name = "Dam type")+
  facet_wrap(~region, scales = "free_y") +
  labs(x = "Model",
       y = "Estimated total number of GLOFs",
       subtitle = "Hindcast for the period 1901-1972") +
  geom_hline(data = all_counts_hindcast %>% filter(type == "all"), 
             aes(yintercept = value),
             size = 1.2, color = "navy") +
  theme_bw() +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 7.5),
        axis.text.x =  element_text(size = 7))


##############################################################################################################
### Forecasting GLOFs from trends in GLOF reporting, and 
### mean annual air temperatures and research activity between 1901 and 1972.
##############################################################################################################

# Now we change the training and test period to learn the same two models as above.
# We thus learn trends in GLOF reporting during the period 1901-1972, and forecast
# the number of GLOF for the subsequent period (1973-2018).

# Define training data set.

dat <- bind_rows(region.list) %>%
  filter((region != "Global") & (region != "Other")) %>%
  filter(year >= 1901 & year < 1973) %>%
  pivot_longer(cols = c(moraine, ice, other), names_to = "type", values_to = "count") %>%
  mutate(year_scale = as.numeric(scale(year)),
         temp_scale = as.numeric(scale(temp_mean)),
         meas_scale = as.numeric(scale(log10(mb_and_front+1))))

# Define test data set.

dat2 <- bind_rows(region.list) %>%
  filter((region != "Global") & (region != "Other")) %>%
  filter(year >= 1973 & year < 2018) %>%
  pivot_longer(cols = c(moraine, ice, other), names_to = "type", values_to = "count") %>%
  mutate(meas_scale = (log10(mb_and_front+1) - mean(log10(dat$mb_and_front+1))) / sd(log10(dat$mb_and_front+1)),
         temp_scale = (temp_mean - mean(dat$temp_mean)) / sd(dat$temp_mean),
         year_scale =  (year - mean(dat$year)) / sd(dat$year)) 



# We maintain the choice of robust priors on the intercepts and slopes, and normal priors 
# on the standard deviations and the zero inflation probability. 
# Maintain default priors on all other parameters. 

bprior <- prior(student_t(3, 0, 2.5), class = "Intercept") + 
  prior(student_t(3, 0, 2.5), class = "b", coef = "meas_scale") + 
  prior(student_t(3, 0, 2.5), class = "b", coef = "temp_scale") + 
  prior(normal(0, 2.5), class = "sd") +
  prior(normal(5, 3), class = "sd", dpar = "zi") 

# Run the hierarchical Poisson regression model to estimate GLOF counts
# from mean annual air temperatures and the annual number of surveyed glaciers.

fit.temp.meas.before.1973  <- brm(
  bf(count ~ temp_scale + meas_scale + 
       ( temp_scale + meas_scale | type:region), 
     zi ~ 1 + (1 | type:region)),
                  family = zero_inflated_poisson(),
                  data = dat,
                  prior = bprior,
                  cores = 4,
                  chains = 4,
                  warmup = 1500,
                  iter = 3500 ,
                  control = list(adapt_delta = 0.97,
                                 max_treedepth = 12))

# Summarise the model fit.

summary(fit.temp.meas.before.1973 )
plot(fit.temp.meas.before.1973 )
pp_check(fit.temp.meas.before.1973 )

# The following lines of code are similar to the models above.
# We generate posterior distributions of the weights on 
# temperature and glacier surveys in the model. We then 
# plot the trend of GLOF, here depending on the annual number of
# glacier surveys and keeping temperatures fixed.

conds <- dat %>% 
  group_by(region) %>% 
  summarise(min_r = min(meas_scale), 
            max_r = max(meas_scale))

preds <- dat %>%
  data_grid(temp_scale = 0,
            meas_scale = seq_range(meas_scale, n = 101),
            type       = unique(dat$type),
            region     = unique(dat$region)) %>%
  add_fitted_draws(model = fit.temp.meas.before.1973, 
                   value = "count", 
                   n = 2000)

preds.sub <- list()

for (i in 1:nrow(conds)) {
  
  preds.sub[[i]] <- filter(preds, 
                           region == conds$region[i] & 
                             meas_scale >= conds$min_r[i] &  
                             meas_scale <= conds$max_r[i])
}

preds.sub <- bind_rows(preds.sub)


plot_trend_temp_bef_1973 <- preds.sub %>%
  mutate(meas = (meas_scale * sd(log10(dat$mb_and_front+1)))
         + mean(log10(dat$mb_and_front+1))) %>%
  ggplot(aes(x = meas, y = count, color = type)) +
  facet_wrap(~region, scales = "free", ncol = 2) +
  scale_color_manual(name = "Dam type", values = cividis(3)) +
  scale_fill_manual(name = "Posterior rate", values = "#52c8c8c8") +
  stat_lineribbon(aes(y = count), .width = 0.95,
                  point_interval = mean_qi) +
  geom_point(data = dat,
             aes(x = log10(mb_and_front+1), y = count, color = type),
             shape = 16) +
  theme_bw() +
  labs(x = "Annual number of glaciological surveys [log10]",
       y = "GLOF reporting rate")

# Posterior distribution of measurements

fit.temp.meas.before.1973 %>%
  spread_draws(b_meas_scale, `r_type:region`[region, param]) %>%
  filter(param == "meas_scale") %>%
  mutate( region_mean = b_meas_scale + `r_type:region`) %>%
  summarise(quantslow = quantile(region_mean, 0.025), 
            quantsup = quantile(region_mean, 0.975))


mod_param_meas_bef_1973 <- fit.temp.meas.before.1973 %>%
  spread_draws(b_meas_scale, `r_type:region`[region, param]) %>%
  filter(param == "meas_scale") %>%
  mutate( region_mean = b_meas_scale + `r_type:region`) %>%
  ungroup() %>%
  mutate(region = str_replace_all(region, "[.]", "\n")) %>%
  separate(col = region, into = c("type", "region2"), sep ="_") %>%
  mutate(region2 = as_factor(region2) %>% fct_rev(),
         type = as_factor(type) %>% fct_rev()) %>%
  ggplot(aes(x = region_mean,
             y = region2,
             fill = type)) +
  stat_halfeye(shape = 21,
               point_size = 3,
               slab_alpha = 0.5,
               slab_color = "gray25",
               interval_size = 2,
               interval_color = "black",
               position = position_dodge(width = .6)) + 
  scale_fill_viridis_d(option = "E",
                       direction = -1,
                       name = "Dam type") + 
  theme_bw() +
  labs(x = "Standardised\nregression slopes\nGlac. surveys",
       y = "") +
  geom_vline(xintercept = 0)

# Posterior distribution of temperatures

fit.temp.meas.before.1973 %>%
  spread_draws(b_temp_scale, `r_type:region`[region, param]) %>%
  filter(param == "temp_scale") %>%
  mutate( region_mean = b_temp_scale + `r_type:region`) %>%
  summarise(quantslow = quantile(region_mean, 0.025), 
            quantsup = quantile(region_mean, 0.975))

mod_param_temp_bef_1973 <- fit.temp.meas.before.1973 %>%
  spread_draws(b_temp_scale, `r_type:region`[region, param]) %>%
  filter(param == "temp_scale") %>%
  mutate( region_mean = b_temp_scale + `r_type:region`) %>%
  ungroup() %>%
  mutate(region = str_replace_all(region, "[.]", "\n")) %>%
  separate(col = region, into = c("type", "region2"), sep ="_") %>%
  mutate(region2 = as_factor(region2) %>% fct_rev(),
         type = as_factor(type) %>% fct_rev()) %>%
  ggplot(aes(x = region_mean,
             y = region2,
             fill = type)) +
  coord_cartesian(xlim = c(-3, 5)) + 
  stat_halfeye(shape = 21,
               point_size = 3,
               slab_alpha = 0.5,
               slab_color = "gray25",
               interval_size = 2,
               interval_color = "black",
               position = position_dodge(width = .6)) + 
  scale_fill_viridis_d(option = "E",
                       direction = -1,
                       name = "Dam type") + 
  theme_bw() +
  labs(x = "Standardised\nregression slopes\nAir temperatures",
       y = "") +
  geom_vline(xintercept = 0)

# Paste all figures togethe to generate Fig. S4.

p_trends_bef_1973 <- ggarrange(
  plotlist = list(plot_trend_temp_bef_1973, 
                  mod_param_temp_bef_1973,  
                  mod_param_meas_bef_1973),
  ncol   = 3,
  widths = c(2, 1,1),
  font.label = list(size = 10),
  legend = "bottom",
  labels = "auto",
  label.x = 0,
  label.y = 1,
  hjust = -0.5,
  vjust = 1.5,
  align = "v",
  common.legend = T
)

# Write the plot to disk.

ggsave(filename = "New_Plots/glof_trends_temp_meas_before_1973.pdf",
       plot   = p_trends_bef_1973,
       width  = 240, 
       height = 150, 
       units  = "mm")

# Generate the forecasts from temperatures and glacier surveys, 
# same code as for the hindcasts. 

pred.test.3 <- brms:::predict.brmsfit(fit.temp.meas.before.1973, 
                                      newdata = dat2, 
                                      summary = F)
pred.test.3 <- t(pred.test.3)  
tt.3 <- bind_cols(Region = dat2$region, 
                  type =     dat2$type,
                  year =     dat2$year, 
                  as_tibble(pred.test.3))


sim_by_reg.3 <- list()

for(i in unique(tt.3$Region)) {
  
  all <- tt.3 %>% 
    filter(Region == i) %>%
    select(starts_with("V")) %>%
    colSums() %>% 
    as_tibble() %>%
    mutate(type = "all",
           region = i)
  
  for(j in unique(tt.3$type)) {
    
    reg <- tt.3 %>%
      filter(Region == i & type == j) %>%
      select(starts_with("V")) %>%
      colSums()  %>% 
      as_tibble() %>%
      mutate(type = j ,
             region = i)
    
    all <- bind_rows(all, reg)
    
  }
  
  sim_by_reg.3[[i]] <- all
  
}

sims.3 <- bind_rows(sim_by_reg.3) %>%
  mutate(model = "Temp.\n+ Glac. surv.")  %>%
  group_by(region, type) %>%
  filter( (value <= quantile(value, 0.975))) %>%
  ungroup()


##################################################################

# Finally, we wish to learn the trends in GLOF reporting for the period
# 1901-1972. We use the same priors as above, that is robust priors
# on the intercept and normal priors on the group-level effects and
# the zero-inflation probability.

bprior <- prior(student_t(3, 0, 2.5), class = "Intercept") + 
  prior(student_t(3, 0, 2.5), class = "b") + 
  prior(normal(0, 2.5), class = "sd") +
  prior(normal(5, 3), class = "sd", dpar = "zi") 


# Run the hierarchical model to estimate trends in GLOF reporting over time.

fit.trend.before.1973  <- brm(
                  bf(count ~ year_scale +  ( year_scale | type:region),
                    zi ~ 1 + (1 | type:region)),
                  family = zero_inflated_poisson(),
                  data   = dat,
                  prior  = bprior,
                  cores  = 4,
                  chains = 4,
                  warmup = 1500,
                  iter   = 3500 ,
                  control = list(adapt_delta = 0.97,
                                 max_treedepth = 12))


# Code to generate Fig. S5.

conds <- dat %>% 
  group_by(region) %>% 
  summarise(min_r = min(year_scale), 
            max_r = max(year_scale))

preds <- dat %>%
  data_grid(year_scale = seq_range(year_scale, n = 101),
            type       = unique(dat$type),
            region     = unique(dat$region)) %>%
  add_fitted_draws(model = fit.trend.before.1973, 
                   value = "count", 
                   n = 1000)

preds.sub <- list()

for (i in 1:nrow(conds)) {
  
  preds.sub[[i]] <- filter(preds, 
                           region == conds$region[i] & 
                             year_scale >= conds$min_r[i] &  
                             year_scale <= conds$max_r[i])
}

preds.sub <- bind_rows(preds.sub)


plot_trend_year_before <- preds.sub %>%
  mutate(year = year_scale * sd(dat$year) + mean(dat$year)) %>%
  ggplot(aes(x = year, y = count, color = type)) +
  facet_wrap(~region, scales = "free", ncol = 2) +
  geom_point(data = dat,
             aes(x = year, y = count, color = type),
             shape = 16) +
  scale_color_manual(name = "Dam type", values = cividis(3)) +
  scale_fill_manual(name = "Posterior rate", values = "#52c8c8c8") +
  stat_lineribbon(aes(y = count), .width = 0.95,
                  point_interval = mean_qi) +
  theme_bw() +
  labs(x = "Year",
       y = "Number of GLOFs")


# Posterior 95% HDI of GLOF reporting trends.

fit.trend.before.1973 %>%
  spread_draws(b_year_scale, `r_type:region`[region, param]) %>%
  filter(param == "year_scale") %>%
  mutate( region_mean = b_year_scale + `r_type:region`) %>% 
  group_by(region) %>% 
  summarise(quantslow = quantile(region_mean, 0.025), 
            quantsup = quantile(region_mean, 0.975))

# Also as plot.

mod_param <- fit.trend.before.1973 %>%
  spread_draws(b_year_scale, `r_type:region`[region, param]) %>%
  filter(param == "year_scale") %>%
  mutate( region_mean = b_year_scale + `r_type:region`) %>%
  ungroup() %>%
  mutate(region = str_replace_all(region, "[.]", "\n")) %>%
  separate(col = region, into = c("type", "region2"), sep ="_") %>%
  mutate(region2 = as_factor(region2) %>% fct_rev(),
         type = as_factor(type) %>% fct_rev()) %>%
  ggplot(aes(x = region_mean,
             y = region2,
             fill = type)) +
  coord_cartesian(xlim = c(-2, 2)) + 
  stat_halfeye(shape = 21,
               point_size = 1.5,
               slab_alpha = 0.5,
               slab_color = "gray25", slab_size = 0.5,
               interval_size = 2,
               interval_color = "black",
               position = position_dodge(width = .6)) + 
  scale_fill_viridis_d(option = "E",
                       direction = -1,
                       name = "Dam type") + 
  theme_bw() +
  labs(x = "Standardised\nregression slope",
       y = "Region") +
  geom_vline(xintercept = 0)

# Combine the trends and the posterior parameter values of the slope to generate Fig. 5.

p_trend_before_1973 <- ggarrange(
  plotlist = list(plot_trend_year_before, mod_param),
  ncol = 2,
  font.label = list(size = 10),
  legend = "bottom",
  labels = "auto",
  label.x = 0,
  widths  = c(2,1),
  label.y = 1,
  hjust = -0.5,
  vjust = 1.5,
  align = "h",
  common.legend = T
)

# Write the plot to disk.

ggsave(filename = "New_Plots/glof_trends_before_1973.pdf",
       plot =  p_trend_before_1973,
       width = 180, 
       height = 120, 
       units = "mm")


# Generate the forecasts from the trends in the period 1901-1972, 
# same code as for the hindcasts. 

pred.test.4 <- brms:::predict.brmsfit(fit.trend.before.1973, 
                                      newdata = dat2, 
                                      summary = F)
pred.test.4 <- t(pred.test.4)  
tt.4 <- bind_cols(Region = dat2$region, 
                  type   = dat2$type,
                  year   = dat2$year, 
                  as_tibble(pred.test.4))


sim_by_reg.4 <- list()

for(i in unique(tt.4$Region)) {
  
  all <- tt.4 %>% 
    filter(Region == i) %>%
    select(starts_with("V")) %>%
    colSums() %>% 
    as_tibble() %>%
    mutate(type = "all",
           region = i)
  
  for(j in unique(tt.4$type)) {
    
    reg <- tt.4 %>%
      filter(Region == i & type == j) %>%
      select(starts_with("V")) %>%
      colSums()  %>% 
      as_tibble() %>%
      mutate(type = j ,
             region = i)
    
    all <- bind_rows(all, reg)
    
  }
  
  sim_by_reg.4[[i]] <- all
  
}

sims.4 <- bind_rows(sim_by_reg.4) %>%
  mutate(model = "Year")  %>%
  group_by(region, type) %>%
  filter( (value <= quantile(value, 0.975))) %>%
  ungroup()


# Summarise true counts

true_counts_forecast <- dat2 %>% 
  group_by(region, type) %>% 
  summarise(value = sum(count)) 

all_counts_forecast <- true_counts_forecast %>% 
  group_by(region) %>% 
  summarise(value =sum(value)) %>% 
  mutate(type = "all") %>%
  bind_rows(true_counts_forecast) %>%
  mutate(region = ifelse(region == "High Mountain Asia", "HMA", region))

# Combine both forecast models into one plot.

forecast_temp_glsurv_and_year <- bind_rows(sims.3, sims.4) %>% 
  filter(type == "all") %>%
  mutate(region = ifelse(region == "High Mountain Asia", "HMA", region)) %>%
  ggplot(aes(model, value, fill = model)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), 
              scale = "width", 
              color = "black") +
  scale_fill_manual(values = c("darkorange", "darkorange3" ),
                    name = "Dam type")+
  facet_wrap(~region, scales = "free_y") +
  labs(x = "Model",
       y = "Estimated total number of GLOFs",
       subtitle = "Forecast for the period 1973-2017") +
  geom_hline(data = all_counts_forecast %>% filter(type == "all"), 
             aes(yintercept = value),
             size = 1.2, color = "navy") +
  theme_bw() +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 7.5),
        axis.text.x =  element_text(size = 7))

# Finally, put the hindcasts and forecasts into one model to generate Fig. 4.

hind_and_forecast <- ggarrange(
  plotlist = list(hindcast_temp_glsurv_and_year, forecast_temp_glsurv_and_year),
  ncol = 2,
  font.label = list(size = 10),
  legend = "none",
  labels = "auto",
  label.x = 0,
  label.y = 1,
  hjust = -0.5,
  vjust = 1.5,
  align = "hv")

# Save the figure to disk.

ggsave(filename = "New_Plots/hind_and_forecasts.pdf",
       plot   = hind_and_forecast ,
       width  = 210, 
       height = 100, 
       units  = "mm")


############################################################################################

# Summarise the total predicted amount of GLOFs from the hindcasts.

# Hindcast: observed "true" GLOF counts

all_counts_hindcast %>% filter(type == "all") %>% summarise(tot_count = sum(value))

# simulated - hindcast with Temperature and Glacier surveys

bind_rows(sim_by_reg) %>% 
  filter(type == "all") %>% 
  group_by(region) %>% 
  mutate(id = 1:n()) %>% 
  pivot_wider(names_from = id) %>%
  ungroup() %>%
  select(!c(type, region)) %>%
  colSums() %>% 
  as_tibble() %>%
  quantile(c(.025, 0.5, 0.975), na.rm = T)

# Only for the European Alps.

sims %>% 
  filter(region == "European Alps") %>% 
  filter(type == "all") %>% 
  summarise(quantile(value, c(.025, 0.5, 0.975), na.rm = T))


# simulated - hindcast with year only

bind_rows(sim_by_reg.2) %>% 
  filter(type == "all") %>% 
  group_by(region) %>% 
  mutate(id = 1:n()) %>% 
  pivot_wider(names_from = id) %>%
  ungroup() %>%
  select(!c(type, region)) %>%
  colSums() %>% 
  as_tibble() %>%
  quantile(c(.025, 0.5, 0.975), na.rm = T)


# Forecast: observed "true" GLOF counts

all_counts_forecast %>% filter(type == "all") %>% summarise(tot_count = sum(value))

# simulated - forecast with temp and glac. surveys

bind_rows(sim_by_reg.3) %>%
  filter(type == "all") %>% 
  group_by(region) %>%
  mutate(id = 1:n()) %>% 
  pivot_wider(names_from = id) %>%
  ungroup() %>%
  select(!c(type, region)) %>%
  colSums() %>% 
  as_tibble() %>%
  quantile(c(.025, 0.5, 0.975), na.rm = T)

# simulated - forecast with year only

bind_rows(sim_by_reg.4) %>%
  filter(type == "all") %>% 
  group_by(region) %>%
  mutate(id = 1:n()) %>%  
  pivot_wider(names_from = id) %>%
  ungroup() %>%
  select(!c(type, region)) %>%
  colSums() %>% 
  as_tibble() %>%
  quantile(c(.025, 0.5, 0.975), na.rm = T)



#########################################################################################################

# Generate Fig. S2.

# Annual GLOF counts for each region

plot_a <- bind_rows(region.list) %>%
  filter((region != "Global") & (region != "Other")) %>%
  filter(year >= 1901 & year < 2018) %>%
  mutate(freq = freq - volc) %>%
  mutate(region = as_factor(region) %>% fct_relevel(
    "Andes",
    "Pacific NW",
    "High Mountain Asia",
    "Iceland", 
    "Scandinavia",
    "European Alps")) %>%
  ggplot(aes(fill= region, y = freq, x=year)) + 
  geom_bar(position="stack", stat= "identity", width = 1) + 
  scale_fill_viridis_d(#direction = -1, 
    option = "inferno", "Region") + 
  labs(x = "Year",
       y = "Number of reported GLOFs") +
  theme_bw() +
  theme(axis.text  = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.text= element_text(size = 7))

# Prepare plot b: temperatures per region.

# Obtain global mean annual temperatures

glob.temp <- bind_rows(region.list) %>%
  filter(region == "Global") %>%
  filter(year >= 1901 & year < 2018)

# and regional mean annual air tempetures.

reg.temp <- bind_rows(region.list) %>%
  filter((region != "Global") & (region != "Other")) %>%
  filter(year >= 1901 & year < 2018) %>%
  group_by(year) %>%
  summarise(temp_min = min(temp_mean),
            temp_max = max(temp_mean))

glob.temp <- left_join(glob.temp, reg.temp, by = 'year')

p <- glob.temp %>%
  ggplot(aes(y = temp_mean, x=year)) + 
  geom_ribbon(aes(ymax = temp_max, ymin = temp_min),
              fill = "gray85") +
  geom_line(size = 1.4) +
  theme_bw()

dat <- bind_rows(region.list) %>%
  filter((region != "Global") & (region != "Other")) %>%
  filter(year >= 1901 & year < 2018) %>%
  mutate(region = as_factor(region) %>% fct_relevel(
    "Andes",
    "Pacific NW",
    "High Mountain Asia",
    "Iceland", 
    "Scandinavia",
    "European Alps")) 

# Draw plot b.

plot_b <- p + 
  geom_line(data = dat, aes(colour = region, y = temp_mean, x=year , group = region)) + 
  scale_colour_viridis_d(# direction = -1, 
    option = "inferno", 
    alpha = 0.5) +
  labs(x = "Year",
       y = "Mean annual air temperatures") +
  theme_bw() +
  theme(axis.text  = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.text= element_text(size = 7))

# Prepare plot c: Annual number of glacier surveys per region.

p <- bind_rows(region.list) %>%
  filter((region != "Global") & (region != "Other")) %>%
  filter(year >= 1901 & year < 2018) %>%
  group_by(year) %>%
  summarise(all_meas = sum(mb_and_front)) %>%
  ggplot(aes(y = all_meas, x = year)) + 
  geom_bar(stat="identity", 
           width = 1,
           fill = "gray60") +
  theme_bw() 

dat <- bind_rows(region.list) %>%
  filter((region != "Global") & (region != "Other")) %>%
  filter(year >= 1901 & year < 2018) %>%
  mutate(all_meas = if_else(all_meas == 0, 1, all_meas)) %>%
  mutate(region = as_factor(region) %>% fct_relevel(
    "Andes",
    "Pacific NW",
    "High Mountain Asia",
    "Iceland", 
    "Scandinavia",
    "European Alps"))

plot_c <- p + geom_point(aes(color = region, y = mb_and_front, x=year), 
                         data = dat,
                         size = 0.4, shape = 16) + 
  scale_color_viridis_d(option = "inferno") +
  labs(x = "Year",
       y = "Glaciological measurements") +
  theme_bw() +
  theme(axis.text  = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.text= element_text(size = 7))

# Combine all panels into one plot to plot Fig. S2.

p_fin <- ggarrange(
  plotlist = list(plot_a, plot_b, plot_c),
  ncol    = 1,
  nrow    = 3,
  legend  = "bottom",
  labels  = "auto",
  label.x = 0,
  label.y = 1,
  hjust   = -0.5,
  vjust   = 1.5,
  align   = "hv",
  font.label = list(size = 8),
  common.legend = T
)

# Save plot to disk.

ggsave(filename = "New_Plots/Counts_temp_meas.pdf",
       plot =  p_fin,
       width = 70, height = 160, units = "mm")


######## FIN! ################
