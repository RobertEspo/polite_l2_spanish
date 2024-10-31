# --------------------------------------------------------------------------- #
# Synthetic data for model testing
# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# Load libs
# 
# --------------------------------------------------------------------------- #

pilot <- read_csv(here("data","pilot","test00","test00_output.csv")) %>%
  na.omit()

# standardize pitch
pilot_z <- pilot %>%
  mutate(
    boundary_pitch_z = scale(boundary_pitch),
    boundary_pitch_z = as.numeric(boundary_pitch_z),
    
    utterance_pitch_z = scale(utterance_pitch),
    utterance_pitch_z = as.numeric(utterance_pitch_z),
    
    # factor
    power = as.factor(power),
    distance = as.factor(distance),
    imposition = as.factor(impositio)
  )

# set priors
common_prior <- c(
  set_prior("normal(0,.5)", class = "b")
)

priors <- c(
  common_prior,
  prior(cauchy(0, 0.1), class = sd), 
  prior(lkj(8), class = cor)
)

# models

# boundary pitch models
pilot_mod_0 <- brm(
  formula = boundary_pitch_z ~ 0,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

pilot_mod_1 <- brm(
  formula = boundary_pitch_z ~ 0 + power,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

pilot_mod_2 <- brm(
  formula = boundary_pitch_z ~ 0 + distance,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

pilot_mod_3 <- brm(
  formula = boundary_pitch_z ~ 0 + imposition,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)


# boundary pitch model viz
conditional_effects(pilot_mod_1)
conditional_effects(pilot_mod_2)
conditional_effects(pilot_mod_3)

# IP pitch models

pilot_mod_4 <- brm(
  formula = utterance_pitch_z ~ 0,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

pilot_mod_5 <- brm(
  formula = utterance_pitch_z ~ power,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

pilot_mod_6 <- brm(
  formula = utterance_pitch_z ~ distance,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

pilot_mod_7 <- brm(
  formula = utterance_pitch_z ~ imposition,
  data = pilot_z,
  prior = common_prior,
  family = gaussian(),
  chains = 4,
  warmup = 2000,
  iter = 4000
)

# IP pitch model viz
conditional_effects(pilot_mod_5)
conditional_effects(pilot_mod_6)
conditional_effects(pilot_mod_7)
