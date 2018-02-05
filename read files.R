#find transfer temp

# prereq ------------------------------------------------------------------

require(rhdf5)
require(multidplyr)
require(tidyverse)

dir <- 'data/'



# functions ---------------------------------------------------------------


my_prep_pairs <- function(pairs) {
  str_extract_all(pairs,'[0-9]',simplify = F) %>%
    unlist() %>% 
    .[c(3,4)] %>% 
    str_c(sep = '_',collapse = '_')
}

my_read <- function(fol) {
  data1 <- h5read(fol,'simulation')
  data_param <- h5read(fol,'parameters')
  df <- tibble(temp = data_param$T,
               pairs = data1$results$`Spin Correlations`$labels,
               mean = data1$results$`Spin Correlations`$mean$value,
               error = data1$results$`Spin Correlations`$mean$error) %>% 
    rowwise() %>% 
    mutate(pairs = my_prep_pairs(pairs)) %>% 
    ungroup() %>% 
    separate( 'pairs',c('x','y'))
}
my_prep_plot <- function(df,t) {
  temp1 <- df %>% 
    filter(temp == t) %>% 
    .$mean
  dim(temp1) <- c(NROW(unique(df$x)),NROW(unique(df$y)))
  return(temp1)
}


my_read_parameters <- function(fol){
  h5read(fol,'parameters') %>% 
    enframe(name = 'parameter') %>%
    mutate(value = map(value,as.character)) %>% 
    unnest(value)
  
}
my_read_sim <- function(fol) {
  data1 <- h5read(fol,'simulation')$results %>% 
    enframe(name = 'type') %>% 
    mutate(value = map(value,enframe,name = 'parameter')) 
  return(data1)
}





# read files --------------------------------------------------------------

str_time <- proc.time()
cl <- 3
cluster <- create_cluster(cl)

df_init_par <- tibble(dir = list.files(dir,pattern = '.out.h5',full.names = T),
                      id = rep(1:cl,length.out = NROW(dir))) %>% 
  mutate(init_par = map(dir,my_read_parameters)) %>% 
  unnest(init_par) %>% 
  filter(parameter %in% c('L','T')) %>% 
  spread(parameter,value)

df_data <- tibble(dir = list.files(dir,pattern = '.out.h5',full.names = T)) %>% 
  mutate(results = map(dir,my_read_sim))  %>% 
  unnest(results) 


df_data1 <- df_data %>% 
  filter(type %in% c("Staggered Magnetization^2","Staggered Magnetization^4")) %>% 
  unnest(value) %>% 
  filter(parameter %in% c('mean')) %>% 
  select(-c(parameter) ) %>% 
  mutate(res = map(value,enframe)) %>% 
  unnest(res) %>% 
  unnest(value) %>% 
  spread(name,value)

df <- left_join(df_init_par,df_data1) %>% 
  select(-dir) %>% 
  arrange(as.numeric(L)) %>% 
  mutate(L = as_factor(L))


df %>%
  mutate(`T` = as.numeric(`T`)) %>% 
  group_by(L,`T`) %>% 
  mutate(value = value[type == 'Staggered Magnetization^4']/value[type == 'Staggered Magnetization^2']^2) %>% 
  ggplot(aes(`T`,value,col = L)) +
  geom_line() +
  # geom_pointrange(aes(ymax = value + error,ymin = value - error)) + 
  geom_point() +
  
  theme_bw()


