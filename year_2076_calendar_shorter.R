dates <- calendar %>% 
  filter(Nepali_month == "Poush", Nepali_year == "2076") %>% 
  select(English_date) %>% 
  timetk::tk_augment_timeseries_signature() %>%
  left_join(calendar %>% 
              select(English_date, contains("Nepali"), Observance) %>% 
              mutate_at("Observance", function(x)str_wrap(x,width = 18))) %>% 
  mutate(mweek = as.numeric(interaction(-mweek, fct_rev(month.lbl)))) %>% 
  mutate(mweek = case_when(
    # capture the second element of the middle week
    mweek >= (((mweek %>% table())[2:(length(mweek %>% table())-1)] < 7) %>% 
                which())[2] %>% 
      names() %>% 
      as.numeric() ~ mweek - 1,
    TRUE ~ as.numeric(mweek)
  )) %>% 
  mutate(mweek = if_else(Nepali_month == "Poush", case_when(
    mweek == 59~3, 
    mweek == 58~2,
    mweek == 57~1, 
    mweek == 1~3, 
    mweek == 2~4, 
    mweek == 3~5, 
    TRUE ~ mweek
  ), mweek)
  )

# reduce all elements equal or great to second element by 1

ggplot(dates, aes(x=wday.lbl, y=mweek)) + 
  geom_tile(colour = "black", fill = "turquoise", alpha = 0.8) +
  geom_text(aes(label=Nepali_day), size = 18, alpha = 0.5, 
            color = "red", check_overlap = FALSE, nudge_y = -0.35) +
  geom_text(aes(label=Observance, family = "Preeti", lineheight = 0.4), size = 7, alpha = 1, 
            color = "blue", check_overlap = FALSE, nudge_y = +0.1) +
  geom_text(aes(label=`Lunar day nepali`, family = "Preeti", 
                lineheight = 0.45), size = 7, alpha = 1, 
            color = "blue", check_overlap = FALSE, angle = 90, nudge_x = -0.42) +
  scale_x_discrete(expand = c(0,0)) +
  # labs(title = paste(month_nep, year_nep, sep = " ")) +
  theme(title = element_text(size = 16), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        panel.background = element_rect(fill = "transparent"), 
        legend.position = "none"
  )
