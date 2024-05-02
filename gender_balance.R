 ## Gender balance
require(pacman)
p_load(dplyr, xlsx, tibble, tidyr, srvyr, purrr, readxl, writexl, ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## load individual level data 22
data_23_rds <- readRDS("data/2023 MSNA cross-crisis indicator-level dataset.RDS")

ind_23 <- data_23_rds$loop
data_23 <- data_23_rds$main %>% filter(resp_age>17) 

ind_23 %>% filter(str_detect(country, "HTI")) %>% count(member_gender)

## to align gender labels across datasets
recode_gender <- function(df=ind_23, col_gender="member_gender"){
  df %>%
    mutate(!!sym(col_gender) := str_replace_all(!!sym(col_gender),
                                                c("^1$"="men", "^2$"="women",
                                                  "^(f|F)emale$"="women", "^(m|M)ale$"="men",
                                                  "^feminin$"="women", "^masculin$"="men",
                                                  "^homme$"="men", "^femme$"="women",
                                                  "prefer_not_to_answer|pnpr|other"="")))
}

## recode age category
ind_23 <- ind_23 %>% recode_gender %>%
  mutate(member_age_cat = case_when(member_age < 18 ~ "0_17", member_age < 41 ~ "18_40", member_age < 60 ~ "41_59", member_age > 59 ~ "60_plus", TRUE ~ NA_character_)) %>% 
  mutate(country=str_replace_all(country, "-", "\n"))

data_23 <- data_23 %>% recode_gender(col_gender = "hoh_gender_cat") %>% recode_gender(col_gender = "resp_gender_cat") %>%
  mutate(resp_age_cat = case_when(resp_age < 18 ~ "0_17", resp_age < 41 ~ "18_40", resp_age < 60 ~ "41_59", resp_age > 59 ~ "60_plus", TRUE ~ NA_character_)) %>%
  mutate(country=str_replace_all(country, "-", "\n"))


country.data <- data_23 %>% filter(!is.na(resp_gender_cat), !is.na(resp_age_cat)) %>% pull(country) %>% unique
country.ind <- ind_23 %>% filter(!is.na(member_gender), !is.na(member_age_cat)) %>% pull(country) %>% unique
country.age.gender <- intersect(country.data, country.ind)

plot.age.pyramid  <- function(df=ind_23, 
                              group_var="country",
                              var_gender="member_gender",
                              var_age_cat="member_age_cat",
                              unit = "Individual",
                              save=F, 
                              value_gender=c("men", "women"),
                              dir = paste0("age_pyramid_ind.csv")){
  df_prop <- df %>% 
    # filter(!!sym(var_gender) %in% value_gender) %>%
    mutate(!!sym(group_var) := toupper(!!sym(group_var))) %>%
    count(!!sym(group_var), !!sym(var_gender), !!sym(var_age_cat)) %>%
    group_by(!!sym(group_var)) %>% mutate(prop=n/sum(n)) %>% filter(!is.na(!!sym(var_age_cat)))
   
  plot <- df_prop %>% ggplot(aes(x = !!sym(var_age_cat), y = prop, fill = !!sym(var_gender))) +
    geom_bar(data = df_prop %>% filter(!!sym(var_gender) == value_gender[1]), stat = "identity", position = "identity") +
    geom_bar(data = df_prop %>% filter(!!sym(var_gender) == value_gender[2]), stat = "identity", position = "identity", aes(y = -prop)) +
    # scale_x_discrete(labels = c("0 17","18 40","41 59","60 plus")) +
    scale_x_discrete(labels = df_prop[[var_age_cat]] %>% unique %>% sort %>% str_replace_all(.,"_"," ")) +
    scale_y_continuous(labels = function(x) {scales::percent(abs(x))}, limits = c(-1,1)) +
    scale_fill_manual(values = c("women"="#EE5859","men"="#0067A9")) +
    labs(title = paste0(unit, " age pyramid by gender"), x = "", y = paste0("% of ", unit, "s"), 
         caption=paste0("Distribution for a total ", sum(df_prop$n), " ", unit, "s who where surveyed"),
         fill=paste0(unit, " gender")) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5, size=9), legend.position = "bottom") +
    coord_flip() + facet_grid(vars(!!sym(group_var)))
  plot
  if (save) write.csv(df_prop, dir, row.names = F)
  return(plot)
}

plot.individual.age <- plot.age.pyramid(df = ind_23 %>% filter(country %in% country.age.gender))
plot.resp.age <- plot.age.pyramid(df=data_23 %>% filter(country %in% country.age.gender),
                                  group_var = "country",
                                  var_gender = "resp_gender_cat",
                                  var_age_cat = "resp_age_cat",
                                  unit = "Respondent",
                                  save = T, dir = "age_pyramid_resp.csv")

plot.age.distrib.msna23 <- gridExtra::grid.arrange(plot.individual.age, plot.resp.age, ncol=2)
ggsave("age.pyramid.2023.png", plot.age.distrib.msna23, bg="white", width=8, height=14)

## male overrepresentation
compare.pop <- function(
    df_resp=data_23,
    col_resp_gender="resp_gender_cat",
    col_resp_age_cat="resp_age_cat",
    group_var="country", 
    df_ind=ind_23,
    col_ind_gender="member_gender",
    col_ind_age_cat="member_age_cat",
    group_var_ind="country",
    value_gender=c("men", "women")){
  df_count_resp <- df_resp %>%
    filter(!!sym(col_resp_gender) %in% value_gender, !!sym(col_resp_age_cat) %in% c("18_40", "41_59", "60_plus")) %>%
    mutate(!!sym(group_var) := toupper(!!sym(group_var))) %>%
    count(!!sym(group_var), !!sym(col_resp_gender), !!sym(col_resp_age_cat)) %>% 
    group_by(!!sym(group_var)) %>% mutate(prop_resp=n/sum(n)) %>% filter(!is.na(!!sym(col_resp_age_cat))) %>%
    rename(n_resp=n) %>% 
    rename(!!sym(group_var_ind):=!!sym(group_var), !!sym(col_ind_gender):= !!sym(col_resp_gender), !!sym(col_ind_age_cat):=!!sym(col_resp_age_cat))
  
  ## same for individual level data
  df_count_ind <- df_ind %>%
    filter(!!sym(col_ind_gender) %in% value_gender, !!sym(col_ind_age_cat) %in% c("18_40", "41_59", "60_plus")) %>%
    mutate(!!sym(group_var_ind) := toupper(!!sym(group_var_ind))) %>%
    count(!!sym(group_var_ind), !!sym(col_ind_gender), !!sym(col_ind_age_cat)) %>%
    group_by(!!sym(group_var_ind)) %>% mutate(prop_ind=n/sum(n)) %>% filter(!is.na(!!sym(col_ind_age_cat))) %>% 
    rename(n_ind=n)

    ## then if you find original population data, plot it as well to have three age pyramid
  df_all <- df_count_resp %>% full_join(df_count_ind) %>% filter(!is.na(n_resp) & !is.na(n_ind)) %>%
    mutate(prop_diff = prop_resp - prop_ind)
  return(df_all)
}

df_compare_pop <- compare.pop()

plot.diff.age <- function(df=df_compare_pop,
                          col_age_cat="member_age_cat",
                          col_prop_diff="prop_diff",
                          col_gender="member_gender",
                          group_var="country",
                          val.women="women",
                          val.men="men"
                          ){
  max <- df_compare_pop$prop_diff %>% abs %>% max %>% round(., 1)
  plot <- df %>%
    ggplot(aes(x=!!sym(col_age_cat), y=!!sym(col_prop_diff), fill=!!sym(col_gender))) +
    geom_bar(data = df, stat = "identity", position = "dodge") +
    scale_x_discrete(labels = unique(df[[col_age_cat]]) %>% gsub("_", " ", .)) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(-max,max)) +
    scale_fill_manual(values = setNames(c("#EE5859", "#0067A9"), c(val.women, val.men))) +
    labs(title = paste0("Difference between proportion of respondents and proportion of individuals\nby age category and gender\n"),
         x = "", y = paste0("\n% difference between respondent and individual"), 
         caption=paste0("Distribution for a total ", sum(df$n_resp), " respondents who where surveyed and\na total of ", 
                        sum(df$n_ind), " individuals in the surveyed households. A positive difference means that respondent are overrepresented compared to the individual sample age/gender breakdown for the age group and gender considered.")  %>% 
           str_wrap(123),
         fill=paste0("Gender")) +
    theme_minimal() + 
    theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5, size=9), legend.position = "bottom") +
    coord_flip() + facet_grid(vars(!!sym(group_var)))
  return(plot)
}
plot
plot.diff <- plot.diff.age()
ggsave("age.pyramid.2023.diff.resp.ind.png", plot.diff, bg="white", width=8, height=12)


