library(dplyr)
library(ggplot2)
library(stringr)
library(readxl)
library(readr)
library(scales)
library(sf)
library(RColorBrewer)

d_daicho <- read_csv("../data/2023-juminkihondaicho.csv")

d_daicho_1 <- d_daicho %>%
  filter(種別 == '人口(計)') %>%
  filter(対象年月 == 202301) %>%
  filter(支所等 == '本区' | 支所等 == '合計') %>%
  filter(区 != '北区' & 区 != '須磨区' & 区 != '西区')
d_daicho_2 <- d_daicho %>%
  filter(種別 == '人口(計)') %>%
  filter(対象年月 == 202301) %>%
  filter(支所等 == '本区' | 支所等 == '合計') %>%
  filter(区 == '北区' | 区 == '須磨区' | 区 == '西区') %>%
  group_by(区) %>% filter(値 == max(値))
d_daicho_districts <- union(d_daicho_1, d_daicho_2)

plot_bar_population <- function(target_area){
  d_daicho_districts %>%
    filter(区 != "全市") %>%
    filter(if(target_area == "全市") TRUE else 区 ==  target_area) %>%
    ggplot() +
      geom_bar(aes(x=reorder(区, -値), y=値), stat="identity", fill="steelblue") +
      geom_text(aes(x=区, y=値, label=scales::comma(値)), vjust=-0.3) +
      ggtitle("神戸市の区ごとの人口数") +
      xlab("区") +
      ylab("人数") +
      scale_y_continuous(labels=comma)
}


map <- read_sf("../data/Shape Files/kobe_districts/all_districts/all_districts.shp")
d_daicho_districts2 <- d_daicho_districts %>% rename(CITY_NAME=区)
map2 <- map %>% 
  mutate(CITY_NAME = str_sub(CITY_NAME, 4,-1)) %>%
  left_join(d_daicho_districts2, by="CITY_NAME") %>% 
  select(CITY_NAME, AREA, 値) %>% rename(population=値)

map_all_wards <-function(target_area){
  head(map2)
  map2 %>% 
    filter(if(target_area == "全市") TRUE else CITY_NAME ==  target_area) %>%
    ggplot() + 
      geom_sf()
}

map_population <- function(target_area){
  map2 %>% 
    filter(if(target_area == "全市") TRUE else CITY_NAME ==  target_area) %>%
    ggplot() +
      geom_sf(aes(fill=population)) +
      scale_fill_gradientn(colors=brewer.pal(11,"GnBu"), labels=comma) +
      theme_void() +
      geom_text(aes(x = 135.18, y = 34.8), label = "北区", color="white", family = "HiraKakuProN-W3") +
      geom_text(aes(x = 135.04, y = 34.72), label = "西区", color="white", family = "HiraKakuProN-W3") +
      geom_text(aes(x = 135.06, y = 34.65), label = "垂水区", color="white", family = "HiraKakuProN-W3") +
      geom_text(aes(x = 135.108, y = 34.67), label = "須磨区", color="black", family = "HiraKakuProN-W3") +
      geom_text(aes(x = 135.215, y = 34.67), label = "中央区", color="black", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.227, y = 34.72), label = "灘区", color="black", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.27, y = 34.72), label = "東灘区", color="white", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.145, y = 34.67), label = "長", color="black", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.147, y = 34.658), label = "田", color="black", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.149, y = 34.646), label = "区", color="black", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.165, y = 34.68), label = "兵", color="black", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.170, y = 34.665), label = "庫", color="black", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.175, y = 34.65), label = "区", color="black", family = "HiraKakuProN-W3", angle=0) +
      labs(fill = "単位：人") +
      ggtitle("神戸市各区の人口")
}

map_population_density <- function(target_area){
  map2 %>% 
    filter(if(target_area == "全市") TRUE else CITY_NAME ==  target_area) %>%
    mutate(population_density = population / AREA) %>% 
    ggplot() + geom_sf(aes(fill=population_density)) +
      scale_fill_gradientn(colors=brewer.pal(11,"GnBu"), labels=comma) +
      theme_void() +
      geom_text(aes(x = 135.18, y = 34.8), label = "北区", color="black", family = "HiraKakuProN-W3") +
      geom_text(aes(x = 135.04, y = 34.72), label = "西区", color="black", family = "HiraKakuProN-W3") +
      geom_text(aes(x = 135.06, y = 34.65), label = "垂水区", color="black", family = "HiraKakuProN-W3") +
      geom_text(aes(x = 135.108, y = 34.67), label = "須磨区", color="black", family = "HiraKakuProN-W3") +
      geom_text(aes(x = 135.215, y = 34.67), label = "中央区", color="black", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.227, y = 34.72), label = "灘区", color="black", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.27, y = 34.72), label = "東灘区", color="black", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.145, y = 34.67), label = "長", color="black", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.147, y = 34.658), label = "田", color="black", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.149, y = 34.646), label = "区", color="black", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.165, y = 34.68), label = "兵", color="white", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.170, y = 34.665), label = "庫", color="white", family = "HiraKakuProN-W3", angle=0) +
      geom_text(aes(x = 135.175, y = 34.65), label = "区", color="white", family = "HiraKakuProN-W3", angle=0) +
      labs(fill = "単位：人/km2") +
      ggtitle("神戸市各区の人口密度")
}



d_city_ward_offices <- read_csv("../data/city_ward_offices.csv")

map_location_city_ward_offices <- function(target_area){
  point_data <- d_city_ward_offices %>% 
    filter(if(target_area == "全市") TRUE else str_detect(住所, target_area))
  
  map2 %>% 
    filter(if(target_area == "全市") TRUE else CITY_NAME ==  target_area) %>%
    ggplot() +
      geom_sf()  +
      geom_point(
        data=point_data,
        aes(x=経度, y=緯度, color=分類),
        stat="identity",
        position="identity",
        alpha=0.5
      ) +
      # ops(legend.position="top")
      scale_color_discrete(breaks=c("市役所", "区役所", "支所")) +
      ggtitle("神戸市内の市役所・区役所・支所の位置")
}
