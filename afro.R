library(tidyverse)
library(glmnet)
library(NbClust)


df <- haven::read_sav("./data/r7_merged_data_34ctry.release.sav")
df <- df %>% filter(COUNTRY == 19)


codebook <- readxl::read_excel("./data/codebook_ab_edit.xlsx")

codebook <- codebook %>% filter(type != "na")

df <- df %>% select(codebook$colname)

recodeOrd <- function(v, code) {
  codes <- code$min:code$max
  if(!is.na(code$mid)) {
    codes <- c(codes, code$mid)
  }
  flip <- code$min > code$max
  v[! v %in% codes] <- NA
  if(!is.na(code$mid)) {
    codes <- code$min:code$max
    lgt <- length(codes) / 2
    rc <- v == code$mid
    rc[is.na(rc)] <- F
    thresh <- sort(codes)[lgt]
    v[(!is.na(v)) & v > thresh] <- v[(!is.na(v)) & v > thresh]+1
    v[rc] <- thresh+1
    if(!flip) {
      code$max <- code$max + 1
    } else {
      code$min <- code$min + 1
    }
    
  }
  range <- abs(code$max - code$min)
  floor <- min(c(code$min, code$max))
  v <- (v - floor)/range
  if(flip) v <- 1-v
  # if(range != 1) {
  #   v <- (v - mean(v, na.rm = T)) / sd(v, na.rm = T)  
  # }
  v
}

makeIndex <- function(df, vars) {
  df %>% 
    select(vars) %>% 
    mutate(id = 1:nrow(.)) %>% 
    pivot_longer(-id) %>% 
    group_by(id) %>% 
    summarize(
      index = sum(value, na.rm = T) / sum(!is.na(value))
    ) %>% mutate(
      index = ifelse(index == Inf, NA, index)
    ) %>% arrange(id) %>% pull(index)
}


for (i in 1:nrow(codebook)) {
  row <- codebook[i,]
  if(row$type2 == "ord") {
    df[[row$colname]] <- recodeOrd(
      df[[row$colname]], 
      row
    )
  }
}

df <- df %>% mutate( 
  Q1 = ifelse(Q1 %in% c(998, 999, -1), NA, Q1), 
  Q2A = recode(
    Q2A %>% as.numeric(), 
    `1500` = "Arabic", 
    `1501` = "Amazigh", 
    `1502` = "Arabic", 
    `1503` = "Amazigh", 
    `1504` = "Amazigh", 
    .default = as.character(NA)
  ) %>% as_factor() %>% fct_relevel("Arabic"), 
  Q59A = recode(
    Q59A %>% as.numeric(), 
    `0` = "President", 
    `1` = "Parliament", 
    `2` = "Party", 
    `3` = "Voters", 
    `4` = "Nobody", 
    .default = as.character(NA)
  ) %>% as_factor() %>% fct_relevel("Parliament"), 
  Q59B = recode(
    Q59B %>% as.numeric(), 
    `0` = "President", 
    `1` = "Parliament", 
    `2` = "Party", 
    `3` = "Voters", 
    `4` = "Nobody", 
    .default = as.character(NA)
  ) %>% as_factor() %>% fct_relevel("Voters"), 
  Q59C = recode(
    Q59C %>% as.numeric(), 
    `0` = "President", 
    `1` = "Parliament", 
    `2` = "Party", 
    `3` = "Voters", 
    `4` = "Nobody", 
    .default = as.character(NA)
  ) %>% as_factor() %>% fct_relevel("Parliament"), 
  Q84 = recode(
    Q84 %>% as.numeric(), 
    `1500` = "Arabic",
    `1501` = "Rifi", 
    `1502` = "Soussi", 
    `1503` = "Chalh", 
    `1504` = "Sahraoui", 
    `1505` = "Mountain", 
    `9990` = "Not ethnic", 
    .default = as.character(NA)
  ) %>% as_factor() %>% fct_relevel("Arabic"), 
  Q94 = case_when( # TODO: standardize
    is.na(Q94) ~ as.numeric(NA), 
    Q94 %in% c(-1, 8, 9) ~ as.numeric(NA), 
    Q94 == 1 ~ 1, 
    T ~ 0
  ), 
  Q95A = as.numeric(Q95A), 
  Q95A = case_when(
    Q95A == 1 ~ "Student", 
    Q95A == 2 ~ "Housewife", 
    between(Q95A, 3, 12) ~ "Work", 
    Q95A == 0 ~ "Other", 
    Q95A == 95 ~ "Other", 
    T ~ as.character(NA)
  ) %>% as_factor() %>% fct_relevel("Work"), 
  Q96A = as.numeric(Q96A), 
  Q96A = case_when(
    Q96A == 1 ~ 1, 
    between(Q96A, 2, 12) ~ 0, 
    T ~ as.numeric(NA)
  ), 
  URBRUR = as.integer(URBRUR == 1), 
  Q22 = as.numeric(Q22), 
  Q22 = case_when(
    Q22 == 1 ~ 1, 
    Q22 == 0 ~ 0, 
    between(Q22, 2, 7) ~ 0, 
    T ~ as.numeric(NA)
  ), 
  EA_ROAD_A = ifelse(EA_ROAD_A == -1, NA, EA_ROAD_A), 
  EA_ROAD_B = ifelse(EA_ROAD_B == -1, NA, EA_ROAD_B), 
  EA_ROAD_A = as.integer(EA_ROAD_A != 6), 
  EA_ROAD_B = as.integer(EA_ROAD_B != 6), 
  Q104 = ifelse(Q104 == -1, NA, Q104), 
  Q104 = as.integer(! Q104 %in% c(1, 4))
)

colnames(df) <- codebook$label

indices <- codebook %>% 
  filter(!is.na(index)) %>% 
  split(.$index) %>% 
  map(~ pull(., label)) %>% 
  imap_dfc(function(vars, name) {
    o <- tibble(v = makeIndex(df, vars))
    colnames(o) <- sprintf("%s_index", name)
    o
  })
  
df <- bind_cols(df, indices)
indexVars <- codebook %>% filter(!is.na(index)) %>% pull(label)
df <- df %>% select(-indexVars)


df <- df %>% filter(!is.na(y))

missingness <- df %>% 
  summarize_all(~ mean(is.na(.))) %>% 
  gather(var, missingness) %>% 
  filter(missingness < .1)

df <- df %>% select(missingness$var)
codebook <- codebook %>% filter(label %in% missingness$var)

dfEst <- df

dfEst <- dfEst %>% mutate_if(is.numeric, function(v) {
  if(length(unique(na.omit(v))) > 2) {
    v <- (v - mean(v, na.rm = T)) / sd(v, na.rm = T)
  }
  v[is.na(v)] <- mean(v, na.rm = T)
  v
})

dfEst <- dfEst %>% mutate_if(is.factor, function(v) {
  maxfac <- table(v) %>% sort(decreasing = T) %>% names()
  maxfac <- maxfac[1]
  v[is.na(v)] <- maxfac
  v
})

df <- df %>% mutate_if(is.numeric, function(v) {
  v[is.na(v)] <- mean(v, na.rm = T)
  v
})
df <- df %>% mutate_if(is.factor, function(v) {
  maxfac <- table(v) %>% sort(decreasing = T) %>% names()
  maxfac <- maxfac[1]
  v[is.na(v)] <- maxfac
  v
})

y <- df$y
w <- df$w
xStd <- dfEst %>% 
  select(codebook %>% filter(type %in% c("demo", "opp")) %>% pull(label)) %>% 
  model.matrix(~ -1 + ., data = .)
x <- df %>% 
  select(codebook %>% filter(type %in% c("demo", "opp")) %>% pull(label)) %>% 
  model.matrix(~ -1 + ., data = .)



set.seed(2021) # election year!

mod <- cv.glmnet(xStd, y, weights = w, type.measure = "mse", family = "binomial")

params <- coef(mod) %>% as.matrix() %>% 
    as_tibble(rownames = "param") %>% 
    filter(s1 != 0, param != "(Intercept)") %>% 
    mutate(esize = abs(s1)) %>% 
    arrange(-esize) %>% 
    mutate(rank = 1:nrow(.)) %>% 
    select(param, coef = s1, rank)


clusters <- xStd[,params$param]

nclust <- NbClust(clusters, method = "kmeans", max.nc = 8)

summary <- as_tibble(x)
summary$cluster <- nclust$Best.partition
summary$w <- df$w
summary$y <- df$y

df1 <- summary %>% select(c("y", params$param, "cluster", "w")) %>% 
  group_by(cluster) %>% 
  summarize_at(vars(-w), ~ weighted.mean(., w))
df1 <- bind_rows(
  df1, 
  summary %>% select(c("y", params$param, "w")) %>% 
    summarize_at(vars(-w), ~ weighted.mean(., w)) %>% 
    mutate(cluster = 0)
) %>% arrange(cluster)
df2 <- summary %>% group_by(cluster) %>% 
  summarize(pct = sum(w)) %>% 
  mutate(pct = pct / sum(pct))
out <- left_join(df1, df2) %>% 
  replace_na(list(pct = 1)) %>% 
  pivot_longer(-cluster, "param") %>% 
  left_join(params) %>% 
  replace_na(list(rank = 0)) %>% 
  arrange(rank) %>% 
  pivot_wider(names_from = cluster, names_prefix = "cluster_", values_from = value)

openxlsx::write.xlsx(
  out, 
  "./output/chap5-afro.xlsx", 
  overwrite = T
)

