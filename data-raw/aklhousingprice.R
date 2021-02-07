# remotes::install_github("Tina-ye112/kiaora")

library(tidyverse)
library(lubridate)

aklhousingprice <- kiaora::nzhousingprice %>%
  filter(
    year(auction_dates) > 2017,
    region != "-",
    auction_price < 2e07,
    auction_price > 3000,
    bedrooms < 20,
    bathrooms < 20
  ) %>%
  filter(
    region == "Auckland",
    auction_price < 10000000,
    rating_value < 10000000
  )

usethis::use_data(aklhousingprice, overwrite = TRUE)
