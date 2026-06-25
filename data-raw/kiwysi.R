# Refit KIWSI compactness model with redistmetrics scores.

devtools::load_all()
library(dplyr)

load('C:/Users/chris/Documents/GitHub/compactness/data/training_labels.RData')

shp <- sf::st_read('data-raw/kiwysi/both.shp', quiet = TRUE)
plans <- seq_len(nrow(shp))

features <- redistmetrics:::kiwysi_features(
  plans = plans,
  shp = shp,
  epsg = 3857,
  ncores = 1
)

training <- features |>
  tibble::as_tibble() |>
  mutate(district = shp$NAME) |>
  left_join(
    train_labels |> select(district, compactness, set),
    by = 'district'
  ) |>
  filter(!is.na(compactness))

kiwysi_formula <- compactness ~
  polsby +
  hull +
  reock +
  bbox +
  box_reock +
  lenwid +
  boyce +
  sym_x +
  sym_y +
  skew +
  corners +
  jagged +
  components +
  holes +
  polsby:hull +
  polsby:corners +
  hull:corners +
  sym_x:sym_y

kiwysi_model <- stats::lm(kiwysi_formula, data = training)

usethis::use_data(kiwysi_model, internal = TRUE, overwrite = TRUE)
