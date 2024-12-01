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
saveRDS(pilot_test01_bmod_0, file = here("models","pilot_models","pilot_test_01_bmod_0.rds"))
saveRDS(pilot_test01_bmod_1, file = here("models","pilot_models","pilot_test_01_bmod_1.rds"))

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
