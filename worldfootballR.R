library(tidyverse)
# library(gt)
Sys.setenv(http_proxy = "172.30.15.242:8080")
Sys.setenv(https_proxy = "172.30.7.242:8080")
# devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)
source("H:/Upside/538_theme.R")
# library(caret)


worldfootballR::fb_league_urls("GER","M","2022","2nd")

xG_matches <- get_match_results(country = "GER", gender = "M", season_end_year = 2022, tier = "1st")
xG_matches_url <- xG_matches %>% select(MatchURL) %>% as_vector()

xG_stats <- get_advanced_match_stats(match_url = "https://fbref.com/en/matches/82c5d82c/Monchengladbach-Bayern-Munich-August-13-2021-Bundesliga", stat_type = "shooting", team_or_player = "player")


player_stats <- worldfootballR::get_season_team_stats(country = "GER", gender = "M", season_end_year = "2022", tier = "1st", stat_type = "shooting")

xG_buli <- fb_big5_advanced_season_stats(season_end_year= 2022, stat_type= "shooting", team_or_player= "player") %>% filter(Comp=="Bundesliga")

xG_buli_plot <- xG_buli %>% filter(Pos %in% c("MF","MF,FW","FW","FW,MF"))

unique(xG_buli$Squad)

xG_buli_temp %>% 
  filter(Mins_Per_90>=10,Gls_Standard>0) %>% 
  ggplot(aes(x=agPer90,y=xgPer90)) + 
  geom_point() + 
  geom_smooth(method="lm")

#######
# Logos

league_tables <- read.csv2("buli_table.csv")

temp <- fotmob_get_season_stats(
  league_id = "54",
  season = "2021/2022",
  stat_type = "xg",
  team_or_player = "team"
)

#####
library(tidyverse)
library(worldfootballR)
sessionInfo()

players <- fotmob_get_match_players(3624630)
salah <- players %>% dplyr::filter(id == "737066")
salah_shotmap <- players %>% 
  dplyr::select(player_id = id, shotmap) %>% 
  tidyr::unnest(shotmap)

library(ggsoccer)
library(sportyR)

salah_shotmap %>% 
  filter(teamId==8722) %>% 
  geom_soccer('fifa', touchline_length = 105, goal_line_length = 68) +
  geom_point(aes(x = x, y = y),
             fill = "yellow",
             shape = 21,
             size = 4)
  # annotate_pitch(colour = "white",
  #                fill   = "springgreen4",
  #                limits = FALSE) +
  # theme_pitch() +
  # theme(panel.background = element_rect(fill = "springgreen4")) +
  # coord_flip(xlim = c(49, 101),
  #            ylim = c(-12, 112)) +
  # ggtitle("Simple shotmap",
          # "ggsoccer example")


bulter <- players %>% filter(id == "626890")
bulter %>% 
  select(player_id = id, stats) %>% 
  unnest(stats)
terodde <- players %>% filter(id == "95095")
terodde %>% 
  select(player_id = id, stats) %>% 
  unnest(stats)
sessionInfo()
temp <- fotmob_get_matches_by_date(date = 20210313)
matches <- fotmob_get_league_matches(
  country = "GER",
  league_name = "2. Bundesliga"
  ) %>% select(id) %>% as_vector()
match_details <- worldfootballR::fotmob_get_match_players(3625000) #%>% tidyr::unnest(stats)
temp <- match_details$stats
reprex::reprex()
##### Buli xG
#####

xG_buli_temp <- xG_buli %>%
  mutate(agPerShot = Gls_Standard/ifelse(Sh_Standard==0,0.0000001,Sh_Standard),
         xgPerShot = xG_Expected/ifelse(Sh_Standard==0,0.0000001,Sh_Standard),
         xgPerSHotDiff = agPerShot-xgPerShot,
         agPer90 = Gls_Standard/ifelse(Mins_Per_90==0,0.0000001,Mins_Per_90),
         xgPer90 = xG_Expected/ifelse(Mins_Per_90==0,0.0000001,Mins_Per_90),
         xgPer90Diff = agPer90-xgPer90
  )

# make the table
bind_rows(xG_buli %>% 
            arrange(G_minus_xG_Expected) %>%
            dplyr::slice(1:10),
          xG_buli %>% 
            arrange(-G_minus_xG_Expected) %>%
            dplyr::slice(1:10) %>% 
            arrange(G_minus_xG_Expected)
          ) %>% 
  left_join(league_tables %>% select(Squad=name,id,logoUrl)) %>% 
  select(Player,id,Gls_Standard,xG_Expected,G_minus_xG_Expected) %>% 
  gt() %>%
  tab_header(title = 'Bundesliga Top & Bottom 10 Expected Goals (xG) Difference') %>% 
  cols_label(
    Player = 'Player',
    id = "Squad",
    # Squad = 'Squad',
    Gls_Standard = "Actual",
    xG_Expected = "Expected",
    G_minus_xG_Expected = "xG Diff"
    ) %>% 
  fmt_number(columns = c(xG_Expected,G_minus_xG_Expected), decimals = 1) %>% 
  tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>%
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>%
  tab_style(style = cell_text(align = 'left'), locations = cells_body(c(Player))) %>%
  tab_spanner(label = 'Goals', columns = c(Gls_Standard, xG_Expected)) %>% 
  tab_source_note(source_note = 'Data: @worldfootballR | plot: @ChristianLohr9') %>% 
  data_color(
    columns = c(G_minus_xG_Expected),
    colors = scales::col_numeric(palette = c('#1a9850',"#ffffbf",'#d73027'), domain = c(-6,6)),
    autocolor_text = FALSE
  ) %>%
  text_transform(
    locations = cells_body(c(id)),
    fn = function(x) web_image(url = paste0("https://images.fotmob.com/image_resources/logo/teamlogo/",x,".png"))
  ) %>% 
  cols_width(c(id) ~ px(45)) %>% 
  tab_options(
    table.font.color = '#252525',
    data_row.padding = '2px',
    row_group.padding = '3px',
    column_labels.border.bottom.color = '#252525',
    column_labels.border.bottom.width = 1.4,
    table_body.border.top.color = '#252525',
    row_group.border.top.width = 1.5,
    row_group.border.top.color = '#999999',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = '#252525',
    table.border.top.color = 'white',
    # table.background.color = '#F2F2F2',
    table.border.bottom.color = 'white',
    row.striping.background_color = '#FFFFFF',
    row.striping.include_table_body = TRUE
  ) %>% 
  gtsave("xG_kickbase.png")


variable_aG <- agPer90
variable_xG <- xgPer90
variable_Diff <- xgPer90Diff

bind_rows(xG_buli_temp %>%
            filter(Mins_Per_90>=10) %>% 
            arrange(xgPer90Diff) %>%
            dplyr::slice(1:10),
          xG_buli_temp %>% 
            filter(Mins_Per_90>=10) %>% 
            arrange(-xgPer90Diff) %>%
            dplyr::slice(1:10) %>% 
            arrange(xgPer90Diff)
) %>% 
  left_join(league_tables %>% select(Squad=name,id,logoUrl)) %>% 
  select(Player,id,agPer90,xgPer90,xgPer90Diff) %>% 
  gt() %>%
  tab_header(title = 'Bundesliga Top & Bottom 10 Expected Goals (xG) per 90 Difference') %>% 
  cols_label(
    Player = 'Player',
    id = "Squad",
    # Squad = 'Squad',
    agPer90 = "Actual",
    xgPer90 = "Expected",
    xgPer90Diff = "xG Diff"
  ) %>% 
  fmt_number(columns = c(agPer90,xgPer90,xgPer90Diff), decimals = 2) %>% 
  tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>%
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body()) %>%
  tab_style(style = cell_text(align = 'left'), locations = cells_body(c(Player))) %>%
  tab_spanner(label = 'Goals', columns = c(agPer90, xgPer90)) %>% 
  tab_source_note(source_note = 'Data: @worldfootballR | Filter: Played at least 900 Minutes | plot: @ChristianLohr9') %>% 
  data_color(
    columns = c(xgPer90Diff),
    colors = scales::col_numeric(palette = c('#1a9850',"#ffffbf",'#d73027'), domain = c(-0.4,0.4)),
    autocolor_text = FALSE
  ) %>%
  text_transform(
    locations = cells_body(c(id)),
    fn = function(x) web_image(url = paste0("https://images.fotmob.com/image_resources/logo/teamlogo/",x,".png"))
  ) %>% 
  cols_width(c(id) ~ px(45)) %>% 
  tab_options(
    table.font.color = '#252525',
    data_row.padding = '2px',
    row_group.padding = '3px',
    column_labels.border.bottom.color = '#252525',
    column_labels.border.bottom.width = 1.4,
    table_body.border.top.color = '#252525',
    row_group.border.top.width = 1.5,
    row_group.border.top.color = '#999999',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = '#252525',
    table.border.top.color = 'white',
    # table.background.color = '#F2F2F2',
    table.border.bottom.color = 'white',
    row.striping.background_color = '#FFFFFF',
    row.striping.include_table_body = TRUE
  ) %>% 
  gtsave("xG_kickbase_per90.png")

#####

######### SPI Ratings
########

spi_raw <- read.csv("H:/GitHub/soccer/soccer-spi/spi_matches.csv")
team_logos <- read.csv2("H:/GitHub/soccer/images/buli2_logos.csv")

spi <- spi_raw %>%
  filter(league_id==1846,
         season==2021)

spi2 <- bind_rows(spi_raw %>%
                    filter(league_id==1846,
                           season==2021
                           ) %>% 
                    select(date,team1,team2,spi1,spi2,score1,score2,xg1,xg2),
                  spi_raw %>%
                    filter(league_id==1846,
                           season==2021
                           ) %>% 
                    select(date,team2=team1,team1=team2,spi2=spi1,spi1=spi2,score2=score1,score1=score2,xg2=xg1,xg1=xg2)) %>% 
  group_by(team1) %>% 
  arrange(date) %>% 
  mutate(game = row_number(),
         scorediff = score1-score2,
         xgdiff = xg1-xg2,
         diffoe = scorediff-xgdiff,
         result = case_when(score1>score2~"W",
                            score1<score2~"L",
                            score1==score2~"T",
                            TRUE~NA_character_),
         scorediff_cum = cumsum(scorediff),
         xgdiff_cum = cumsum(xgdiff),
         diffoe_cum = cumsum(diffoe)) %>% 
  left_join(team_logos %>% rename(logo_home=logoUrl)) %>% 
  left_join(team_logos %>% select(team1,logo_away=logoUrl), by=c("team2"="team1"))

##### Plot League SPI

asp_ratio <- 1.618
width=10

image <- spi2 %>% filter(game==26)

buli2_logos <- spi2 %>% 
  filter(game<27) %>% 
  ggplot(aes(x=game,y=spi1,group=team1)) +
  geom_line(aes(color=color1),size = 1) +
  theme_538() +
  labs(
    x = "Spieltag",
    y = "Soccer Power Index (SPI)",
    title = "Soccer Power Index (SPI) im Saisonverlauf",
    caption = "Data: @fivethirtyeight.com | Plot: @ChristianLohr9"
  ) +
  scale_color_identity()+
  geom_image(aes(image = logoUrl), size = 0.035, by = "width", asp = asp_ratio, data=image)
  
ggsave("buli2_spi_plot.png", buli2_logos, width = width, height = width/asp_ratio, dpi = "retina")

##### Plot Team xgdiff

spi3 <- spi2 %>% 
  mutate(xgdiff=score1-xg1)

buli2_xgdiff <- spi2 %>% 
  filter(!is.na(diffoe),team1=="1. FC Heidenheim 1846") %>% 
  ggplot(aes(x=game,y=diffoe,group=team1)) +
  geom_line(aes(color=color1),size = 1) +
  theme_538() +
  labs(
    x = "Spieltag",
    y = "Goal Difference over Expected (GDOE)",
    title = "Goal Difference over Expected - Schalke 04 2021/22 season",
    subtitle = "Erläuterung: An Spieltag 25 verlor S04 mit 3-4 (ScD = -1), hätte aber nach xG mit 1.53 Toren \ngewinnen müssen: ScDOE = -2.53",
    caption = "Data: @FiveThirtyEight | Plot: @ChristianLohr9"
  ) +
  scale_color_identity()+
  ggimage::geom_image(aes(image = logo_away), size = 0.035, by = "width", asp = asp_ratio)+
  geom_text(aes(x = game + 0.5, y = diffoe, label = result),
            hjust = 0, nudge_x = 0.2, size = 4, fontface = "bold")

ggsave("schalke_ScDOE.png", buli2_xgdiff, width = width, height = width/asp_ratio, dpi = "retina")

#### GT Mean ScDOE League

spi2 %>% 
  group_by(team1) %>% 
  summarise(logo_home = first(logo_home),
            spi = last(spi1),
            scorediff = sum(scorediff,na.rm=TRUE),
            xgdiff = sum(xgdiff,na.rm=TRUE),
            diffoe = sum(diffoe,na.rm=TRUE)
            ) %>% 
  select(-team1) %>% 
  arrange(-diffoe) %>% 
  mutate(Rank=row_number()) %>% 
  gt() %>% 
  tab_header(title = 'Cumulated Goal Difference over Expected (GDOE) - Season 21/22') %>% 
  cols_move_to_start(columns = c(Rank)) %>% 
  cols_label(
    logo_home = "Squad",
    spi = "SPI",
    scorediff = 'Actual',
    xgdiff = 'Expected',
    diffoe = "GDOE"
  ) %>% 
  fmt_number(columns = c(scorediff), decimals = 0) %>% 
  fmt_number(columns = c(xgdiff,diffoe), decimals = 1) %>% 
  tab_spanner(label = 'Goal Difference', columns = c(scorediff, xgdiff)) %>% 
  # tab_style(style = cell_text(size = 'x-large'), locations = cells_title(groups = 'title')) %>% 
  tab_style(style = cell_text(align = 'center', size = 'medium'), locations = cells_body(logo_home)) %>%
  tab_style(style = cell_text(align = 'center'), locations = cells_column_labels(Rank)) %>%
  tab_style(style = cell_text(align = 'center'), locations = cells_body(Rank)) %>%
  # tab_style(style = cell_text(align = 'right'), locations = cells_body(c(scorediff,xgdiff,diffoe))) %>%
  # cols_align(columns = 1,
  #            align = "left") %>%
  tab_footnote(
    footnote = "Soccer Power Index calculated by @FiveThirtyEight",
    locations = cells_column_labels(
      columns = spi
    )
  ) %>% 
  tab_source_note(source_note = 'Data: @FiveThirtyEight | Table: @ChristianLohr9') %>% 
  data_color(
    columns = c(diffoe),
    colors = scales::col_numeric(palette = c('#1a9850',"#ffffbf",'#d73027'), domain = c(-21,21)),
    autocolor_text = FALSE
  ) %>%
  text_transform(
    locations = cells_body(c(logo_home)),
    fn = function(x) web_image(url = x)
  ) %>%
  cols_width(c(logo_home) ~ px(45)) %>%
  tab_options(
    table.font.color = '#252525',
    data_row.padding = '2px',
    row_group.padding = '3px',
    column_labels.border.bottom.color = '#252525',
    column_labels.border.bottom.width = 1.4,
    table_body.border.top.color = '#252525',
    row_group.border.top.width = 1.5,
    row_group.border.top.color = '#999999',
    table_body.border.bottom.width = 0.7,
    table_body.border.bottom.color = '#999999',
    row_group.border.bottom.width = 1,
    row_group.border.bottom.color = '#252525',
    table.border.top.color = 'white',
    # table.background.color = '#F2F2F2',
    table.border.bottom.color = 'white',
    row.striping.background_color = '#FFFFFF',
    row.striping.include_table_body = TRUE
  ) %>% 
  gtsave("H:/GitHub/soccer/output/ScDOE_buli2_sum.png")

#####


# how good is ScDOE in prediction?
#####

# Exploratory analysis
spi2 %>%
  ungroup() %>% 
  select(scorediff, scorediff_cum, xgdiff_cum, diffoe_cum) %>% 
  GGally::ggpairs()

data <- spi2 %>%
  filter(!is.na(scorediff_cum),!is.na(diffoe_cum)) %>% 
  ungroup() %>% 
  select(
    scorediff_cum,
    diffoe_cum
  )

data %>% 
  ggplot(aes(x=scorediff_cum,y=diffoe_cum)) +
  geom_point() +
  geom_smooth(method="lm")

# Data splitting
inTrain <- createDataPartition(
  data$scorediff_cum,
  # data$fpts_perc,
  p = 0.8, list = F)

training <- data[inTrain, ]
testing <- data[-inTrain, ]


# Fit linear regression
fit_lm <- train(
  scorediff_cum ~
  diffoe_cum,
  method = "lm",
  data = training)

# View summary of linear model (optional)
fit_lm$finalModel %>% summary()

# Fit partition tree
fit_rpart <- train(
  scorediff_cum ~
    diffoe_cum,
  method = "xgbTree",
  data = training)


# Get cross-validation samples for model comparison
res <- resamples(list(
  linear_model = fit_lm,
  partition_tree = fit_rpart
))

# Plot model comparison
res %>% bwplot(scales = 'free')

# Try prediction for test set
p <- predict(fit_lm, testing) %>% as_tibble()
q <- predict(fit_rpart, testing) %>% as_tibble()

xxx <- bind_cols(testing %>% select(truth=scorediff_cum),p %>% select(estimate=value))
zzz <- bind_cols(testing %>% select(truth=scorediff_cum),q %>% select(estimate=value)) 

library(tidymodels)
library(gglm)
library(lindia)

metrics(xxx, truth, estimate)
metrics(zzz, truth, estimate)

gglm(lm(
  scorediff_cum ~
    diffoe_cum,
        data = spi2))

gg_diagnose(lm(
  scorediff_cum ~
    diffoe_cum,
  data = spi2))
