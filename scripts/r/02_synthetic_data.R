# --------------------------------------------------------------------------- #
# Synthetic data for model testing
# --------------------------------------------------------------------------- #

# --------------------------------------------------------------------------- #
# Load libs
# 
# --------------------------------------------------------------------------- #

set.seed(123)

n_participants <- 15 # num participants
n_conditions <- 8 # num conditions
n_utterances <- 3 # num utterances per condition

# df of conditions
conditions <- expand.grid(
  power = c(0,1),
  distance = c(0,1),
  level_of_request = c(0,1)
)

power <- rep(conditions$power, each = n_utterances, times = n_participants)
distance <- rep(conditions$distance, each = n_utterances, times = n_participants)
level_of_request <- rep(conditions$level_of_request, each = n_utterances, times = n_participants)

participant_id <- rep(1:n_participants, each = n_conditions * n_utterances)

# arbitrary baseline for boundary tone height in Hz
baseline = 200

# boundary tone height for each condition
boundary_tone_height <- baseline +
  30 * power +
  20 * distance +
  25 * level_of_request +
  rnorm(length(power), mean = 0, sd = 10)

# create synthetic data df
synth_dat <- data.frame(
  participant_id = as.factor(participant_id),
  power = as.factor(power),
  distance = as.factor(distance),
  level_of_request = as.factor(level_of_request),
  boundary_tone_height = boundary_tone_height
)

# standardize boundary tone height
synth_dat_z <- synth_dat %>% mutate(
  boundary_tone_height_z = scale(boundary_tone_height),
  boundary_tone_height_z = as.numeric(boundary_tone_height)
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

# model
synth_mod_1 <- brm(
    formula = boundary_tone_height_z ~ 0 + power * distance * level_of_request +
      (1 | participant_id),
    data = synth_dat_z,
    prior = common_prior,
    family = gaussian(),
    chains = 4,
    warmup = 2000,
    iter = 4000
  )

conditional_effects(synth_mod_1)