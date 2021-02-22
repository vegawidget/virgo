# remotes::install_github("Tina-ye112/kiaora")

library(tidyverse)
library(lubridate)

aklhousingprice <- kiaora::nzhousingprice %>%
  filter(
    year(auction_dates) < 2021,
    region == "Auckland"
  ) %>% 
  select(-region) %>% 
  left_join(kiaora::nzpropertygeo) %>% 
  relocate(c(lon, lat), .after = property_address)

usethis::use_data(aklhousingprice, overwrite = TRUE)
