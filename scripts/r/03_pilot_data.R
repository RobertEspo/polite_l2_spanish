# --------------------------------------------------------------------------- #
# Synthetic data for model testing
# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# Load libs
source(here::here("scripts", "r", "00_libs.R"))
# --------------------------------------------------------------------------- #

# load data
pilot <- read_csv(here("data","pilot","pilot_output.csv")) %>%
  na.omit()

# standardize pitch & factor variables
pilot_z <- pilot %>%
  mutate(
    boundary_hz_z = scale(boundary_hz),
    boundary_hz_z = as.numeric(boundary_hz_z),
    
    utterance_hz_z = scale(utterance_hz),
    utterance_hz_z = as.numeric(utterance_hz_z),
    
    # factor
    participant = as.factor(participant),
    power = as.factor(power),
    distance = as.factor(distance),
    imposition = as.factor(imposition)
  )

### bayesian models

# set priors
common_prior <- c(
  set_prior("normal(0,.5)", class = "b")
)

priors <- c(
  common_prior,
  prior(cauchy(0, 0.1), class = sd), 
  prior(lkj(8), class = cor)
)

# boundary pitch models

pilot_bmod_0 <- brm(
  formula = boundary_hz_z ~ 0,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

pilot_bmod_1 <- brm(
  formula = boundary_hz_z ~ 0 + power,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

pilot_bmod_2 <- brm(
  formula = boundary_hz_z ~ 0 + distance,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

pilot_bmod_3 <- brm(
  formula = boundary_hz_z ~ 0 + imposition,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

pilot_bmod_4 <- brm(
  formula = boundary_hz_z ~ power + distance + imposition,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

# boundary pitch model viz

conditional_effects(pilot_bmod_1)
conditional_effects(pilot_bmod_2)
conditional_effects(pilot_bmod_3)
conditional_effects(pilot_bmod_4)

# utterance pitch models

pilot_bmod_5 <- brm(
  formula = utterance_hz_z ~ 0,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

pilot_bmod_6 <- brm(
  formula = utterance_hz_z ~ power,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

pilot_bmod_7 <- brm(
  formula = utterance_hz_z ~ distance,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

pilot_bmod_8 <- brm(
  formula = utterance_hz_z ~ imposition,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

pilot_bmod_9 <- brm(
  formula = utterance_hz_z ~ power + distance + imposition,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

# IP pitch model viz
conditional_effects(pilot_bmod_6)
conditional_effects(pilot_bmod_7)
conditional_effects(pilot_bmod_8)
conditional_effects(pilot_bmod_9)

### BAYESIAN MODEL ONLY TEST01

pilot_z_test01 <- pilot_z %>%
  filter(
    participant == "test01"
  )

pilot_test01_bmod_0 <- brm(
  formula = utterance_hz_z ~ power + distance + imposition,
  data = pilot_z_test01,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

pilot_test01_bmod_1 <- brm(
  formula = boundary_hz_z ~ power + distance + imposition,
  data = pilot_z_test01,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

summary(pilot_test01_bmod_0)
conditional_effects(pilot_test01_bmod_0) # UTTERANCE
conditional_effects(pilot_test01_bmod_1) # BOUNDARY

# save models
saveRDS(pilot_test01_bmod_0, file = here("models","pilot_models","pilot_test_01_bmod_0.rds")) # UTTERANCE
saveRDS(pilot_test01_bmod_1, file = here("models","pilot_models","pilot_test_01_bmod_1.rds")) # BOUNDARY

### frequentist models

# boundary pitch models

pilot_fmod_0 <- lm(boundary_hz_z ~ power,
                   data = pilot_z)

summary(pilot_fmod_0)

pilot_fmod_1 <- lm(boundary_hz_z ~ distance,
                   data = pilot_z)

summary(pilot_fmod_1)

pilot_fmod_2 <- lm(boundary_hz_z ~ imposition,
                   data = pilot_z)

summary(pilot_fmod_2)

pilot_fmod_3 <- lm(boundary_hz_z ~ power + distance + imposition,
                   data = pilot_z)

summary(pilot_fmod_3)

pilot_fmod_4 <- lm(boundary_hz_z ~ power * distance * imposition,
                   data = pilot_z)

summary(pilot_fmod_4)

# utterance pitch models

pilot_fmod_5 <- lm(utterance_hz_z ~ power,
                   data = pilot_z)

summary(pilot_fmod_5)

pilot_fmod_6 <- lm(utterance_hz_z ~ power + distance + imposition,
                   data = pilot_z)

summary(pilot_fmod_6)

pilot_fmod_7 <- lm(utterance_hz_z ~ power * distance * imposition,
                   data = pilot_z)

summary(pilot_fmod_7)

### FOR SLIDES

utterance <- readRDS(file = here("models","pilot_models","pilot_test_01_bmod_0.rds")) # UTTERANCE
boundary <- readRDS(here("models","pilot_models","pilot_test_01_bmod_1.rds")) # BOUNDARY

summary(utterance)

# Generate conditional effects
effects <- conditional_effects(utterance, effects = c("power", "distance", "imposition"))

effects_data <- bind_rows(
  mutate(effects$power, Predictor = "Power"),
  mutate(effects$distance, Predictor = "Distance"),
  mutate(effects$imposition, Predictor = "Imposition")
)

ggplot(effects_data, aes(x = effect1__, y = estimate__, color = Predictor, group = Predictor)) +
  geom_line(size = 1) +
  geom_point(size = 2) +  # Add points for clarity
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = Predictor), alpha = 0.2, color = NA) +
  labs(
    x = "Predictor Value",
    y = "Estimated Z-Score of Pitch",
    title = "Conditional Effects of Predictors",
    color = "Predictor",
    fill = "Predictor"
  ) +
  theme_minimal()

# Create data frame with regression coefficients and CIs
coefficients <- data.frame(
  Term = c("Intercept", "Power", "Distance", "Imposition"),
  Estimate = c(1.46, -0.33, -0.27, -0.39),
  CI_lower = c(1.26, -0.52, -0.47, -0.59),
  CI_upper = c(1.65, -0.13, -0.08, -0.19)
)

# Create the plot
ggplot(coefficients, aes(x = Estimate, y = Term)) +
  geom_point(size = 3, color = "blue") +  # Point estimate
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, color = "black") +  # CI
  labs(
    x = "Estimate",
    y = "",
    title = "Regression Coefficients with 95% CI"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    plot.title = element_text(size = 16, hjust = 0.5)
  )

## boundary

# Generate conditional effects
effects <- conditional_effects(boundary, effects = c("power", "distance", "imposition"))

effects_data <- bind_rows(
  mutate(effects$power, Predictor = "Power"),
  mutate(effects$distance, Predictor = "Distance"),
  mutate(effects$imposition, Predictor = "Imposition")
)

ggplot(effects_data, aes(x = effect1__, y = estimate__, color = Predictor, group = Predictor)) +
  geom_line(size = 1) +
  geom_point(size = 2) +  # Add points for clarity
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = Predictor), alpha = 0.2, color = NA) +
  labs(
    x = "Predictor Value",
    y = "Estimated Z-Score of Pitch",
    title = "Conditional Effects of Predictors",
    color = "Predictor",
    fill = "Predictor"
  ) +
  theme_minimal()

summary(boundary)


coefficients_df <- data.frame(
  Term = c("Intercept", "Power", "Distance", "Imposition"),
  Estimate = c(1.00, -0.43, -0.20, -0.07),
  CI_lower = c(0.29, -1.08, -0.85, -0.72),
  CI_upper = c(1.68, 0.24, 0.47, 0.57)
)


ggplot(coefficients_df, aes(x = Estimate, y = Term)) +
  geom_point(size = 4, color = "#1f77b4") +  # Point estimates
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, color = "#ff7f0e") +  # Credible intervals
  labs(
    x = "Estimate", 
    y = "", 
    title = "Regression Coefficients with 95% CI"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    plot.title = element_text(size = 16, hjust = 0.5)
  )

# For essay

# boundary

pilot_bmod_4 <- brm(
  formula = boundary_hz_z ~ power + distance + imposition,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

summary(pilot_bmod_4)

conditional_effects(pilot_bmod_4)

effects <- conditional_effects(pilot_bmod_4, effects = c("power", "distance", "imposition"))

effects_data <- bind_rows(
  mutate(effects$power, Predictor = "Power"),
  mutate(effects$distance, Predictor = "Distance"),
  mutate(effects$imposition, Predictor = "Imposition")
)

ggplot(effects_data, aes(x = effect1__, y = estimate__, color = Predictor, group = Predictor)) +
  geom_line(size = 1) +
  geom_point(size = 2) +  # Add points for clarity
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = Predictor), alpha = 0.2, color = NA) +
  labs(
    x = "Predictor Value",
    y = "Estimated Z-Score of Boundary Pitch",
    title = "Conditional Effects of Predictors (Boundary)",
    color = "Predictor",
    fill = "Predictor"
  ) +
  theme_minimal()

coefficients_df_boundary <- data.frame(
  Term = c("Intercept", "Power", "Distance", "Imposition"),
  Estimate = c(0.22, -0.30, -0.13, -0.03),
  CI_lower = c(-0.31, -0.79, -0.64, -0.53),
  CI_upper = c(0.75, 0.21, 0.38, 0.48)
)

ggplot(coefficients_df_boundary, aes(x = Estimate, y = Term)) +
  geom_point(size = 4, color = "#1f77b4") +  # Point estimates
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, color = "#ff7f0e") +  # Credible intervals
  labs(
    x = "Estimate", 
    y = "", 
    title = "Regression Coefficients with 95% CI (Boundary)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    plot.title = element_text(size = 16, hjust = 0.5)
  )

# utterance

pilot_bmod_5 <- brm(
  formula = utterance_hz_z ~ power + distance + imposition,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

summary(pilot_bmod_5)

conditional_effects(pilot_bmod_5)

effects <- conditional_effects(pilot_bmod_5, effects = c("power", "distance", "imposition"))

effects_data <- bind_rows(
  mutate(effects$power, Predictor = "Power"),
  mutate(effects$distance, Predictor = "Distance"),
  mutate(effects$imposition, Predictor = "Imposition")
)

ggplot(effects_data, aes(x = effect1__, y = estimate__, color = Predictor, group = Predictor)) +
  geom_line(size = 1) +
  geom_point(size = 2) +  # Add points for clarity
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = Predictor), alpha = 0.2, color = NA) +
  labs(
    x = "Predictor Value",
    y = "Estimated Z-Score of Utterance Pitch",
    title = "Conditional Effects of Predictors (Utterance)",
    color = "Predictor",
    fill = "Predictor"
  ) +
  theme_minimal()

coefficients_df_utterance <- data.frame(
  Term = c("Intercept", "Power", "Distance", "Imposition"),
  Estimate = c(0.23, -0.16, -0.11, -0.20),
  CI_lower = c(-0.30, -0.66, -0.62, -0.70),
  CI_upper = c(0.75, 0.35, 0.41, 0.32)
)


ggplot(coefficients_df_utterance, aes(x = Estimate, y = Term)) +
  geom_point(size = 4, color = "#1f77b4") +  # Point estimates
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, color = "#ff7f0e") +  # Credible intervals
  labs(
    x = "Estimate", 
    y = "", 
    title = "Regression Coefficients with 95% CI (Utterance)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    plot.title = element_text(size = 16, hjust = 0.5)
  )
