#' ######################################
#' ### INSTALL PACKAGES, LOAD DATASET ###
#' ######################################

## First, install packages if necessary:
pks <- c("rJava", "RWeka", "RWekajars", "tidyverse", "lubridate", "hms", "ggplot2", "dplyr")
if (!all(pks %in% rownames(installed.packages()))) 
  install.packages(pks)

## Second, download the raw data

df <- read.csv(file="dados/sto_challenge.csv",head=TRUE,sep=",")

Sys.setenv("JAVA_HOME"="C:/Program Files/Java/jre1.8.0_191") 
Sys.setenv("WEKA_HOME"="C:/Program Files/Weka-3-8")
library("rJava")
library("RWekajars")
library("RWeka")
WPM("refresh-cache")
WPM("load-package", "optics_dbScan")
dbscan <- make_Weka_clusterer('weka/clusterers/DBSCAN')
library(tidyverse)
library(lubridate)
library(hms) 
library(ggplot2)
pkgconfig::set_config("hms::default_tz"="UTC")



## Identify click sprees ("sessions") based on how far away clicks are in time

identify_clicksprees <- . %>% 
  arrange(id, email_id, timestamp) %>% 
  group_by(id, email_id) %>%
  mutate(
    difftime = timestamp-lag(timestamp, default = 0), 
    difftime = as.numeric(`units<-`(difftime, "secs"))) %>% 
  mutate(clickspree_id = cumsum(difftime > 60*20)) %>% # X seconds x Y minutes
  group_by(clickspree_id, add = T) %>% 
  mutate(
    clickspree_nclicks = n(),
    clickspree_nmins = abs(Reduce(`-`, range(timestamp))),
    clickspree_nmins = as.numeric(`units<-`(clickspree_nmins, "mins")),
    clickspree_ageDays = difftime(Sys.time(), min(timestamp)),
    clickspree_ageDays = as.numeric(`units<-`(clickspree_ageDays, "days"))) %>%
  ungroup


datetime_to_radians_to_cartesian <- function(timestamp) {
  h <- as.hms(timestamp)
  h <- hour(h)+minute(h)/60
  ha <- 2*pi*h/24 # 24 hours = 24x60 = 1440 minutes = 2 pi radians. 
  m <- cbind(x = sin(ha), y = cos(ha))
  return(m)
}

## Cluster the timestamps of a user 

getClusters <- function(timestamp, eps = 0.5, othervars = NULL) {
  m <- datetime_to_radians_to_cartesian(timestamp)
  if (!is.null(othervars)) m <- cbind(m, othervars)
  res <- dbscan(m, c("-E", eps, "-M", 1))
  return(res$class_ids)
}


getSendtime <- function(timestamp, age, nclicks, ntime = NULL) {
  ang <- function(x,y) { 
    z <- x + 1i * y
    res <- 90 - Arg(z) / pi * 180
    res %% 360
  }
  
  m <- datetime_to_radians_to_cartesian(timestamp)
  # Weighted mean within cluster: penalize old clicks and small sprees
  xy <- apply(m, MARGIN = 2, weighted.mean, w = (1/age^1.5)*log(nclicks+1))
  h <- ang(xy[1], xy[2]) / 360 * 24 # cartesian to polar to fractional hour
  wm_time <- hms(hours = trunc(h), minutes = (h - trunc(h)) * 60) # fractional hour to hh:mm:ss
  return(as.numeric(wm_time)) 
}

#' ###########################
#' ### OPTIMIZE SEND TIMES ###
#' ###########################
set.seed(1);df %>% 
  # Date type conversion:
  mutate(timestamp = ymd_hms(timestamp), email_id = ymd(email_id)) %>%
  # Disregard opens, just look at clicks:
  filter(action == "click") %>% select(-action) %>% 
  ## filter by weekday of response
  filter(format(timestamp, "%a")%in%c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sab")) %>%
  ## add click spree ids
  identify_clicksprees %>% 
  group_by(id, email_id, clickspree_id) %>%
  ## take just the beginning timestamp of each clickspree
  filter(timestamp == min(timestamp)) %>% 
  ## cluster data, therefore scale age of response data and one-hot-encode weekdays
  mutate(age = scale(clickspree_ageDays)) %>% bind_cols(as.data.frame(model.matrix(~timestamp-1, select(., timestamp) %>% mutate(timestamp = format(timestamp, "%A"))))) %>% group_by(id) %>% mutate(cluster = getClusters(timestamp, eps=.5, cbind(age, timestampseg, timestampter, timestampqua, timestampqui, timestampsex, timestampsab, timestampdom))) %>% group_by(cluster, add=TRUE) %>% mutate(cluster_n = n()) %>% select(-matches("timestamp.+"), -age) %>%
  ## cluster values? Penalties for old sprees + small clusters + clusters with huge time range
  arrange(id, cluster, clickspree_ageDays) %>% group_by(id, cluster) %>% mutate(cluster_value = 1/(mean(head(clickspree_ageDays, 3))^2)*log(cluster_n+1L)*1e10 * (1/(as.numeric(max(as.hms(timestamp))-min(as.hms(timestamp))))^0)) %>% ungroup %>% 
  ## get optimal sendtime
  group_by(id, cluster) %>% mutate(sendtime = getSendtime(timestamp, clickspree_ageDays, clickspree_nclicks)) %>% ungroup %>% mutate(sendtime = as.hms(sendtime)) ->
  DF

#' ####################
#' ### PLOT RESULTS ###
#' ####################
DF %>%
  ## take sample
  arrange(id, email_id) %>% ungroup %>% filter(id %in% sample(unique(id), 4*4))  %>%
  ## plot
  ggplot(aes(x=as.hms(timestamp), clickspree_nmins, size=clickspree_nclicks, color=clickspree_ageDays)) +
  scale_size(range = c(1.5, 4)) + 
  geom_vline(aes(xintercept = sendtime), data = . %>% filter(cluster_n>1) %>% group_by(id) %>% filter(cluster_value == max(cluster_value)), color="green", size = 1.5, alpha = .5) + 
  geom_rect(mapping = aes(xmin=xmin, xmax=xmax, ymin = -Inf, ymax = Inf), data = . %>% filter(cluster_n>1) %>% group_by(id) %>% filter(cluster_value == max(cluster_value)) %>% group_by(cluster, add=T) %>% dplyr::summarise(xmin = min(as.hms(timestamp)), xmax = max(as.hms(timestamp)), cluster_value = cluster_value[1]), color = "red", fill = NA, inherit.aes=F) + 
  geom_point(aes(shape=format(timestamp, "%A")), alpha=.5) + 
  facet_wrap(~id, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  scale_shape_manual(values = 0:6) + 
  scale_x_time(labels = function(x) substr(x, 1, 5)) +
  ggtitle("Clickers with best cluster (red) and optimized sendtime (green)" )

DF %>%  
  
  mutate(wday = factor(format(timestamp, "%a"), levels=c("dom","seg", "ter", "qua", "qui","sex","sab"))) %>% 
  filter(!is.na(wday)) %>% 
  group_by(id) %>% 
  filter(cluster_value == max(cluster_value, na.rm=T)) %>% 
  ungroup %>% 
  ggplot(aes(as.hms(sendtime))) + 
  geom_density() + 
  scale_x_time(breaks = seq(0,60*60*24,60*60*3), minor_breaks = seq(0,60*60*24,60*60*1)) + 
  theme(axis.text.x = element_text(angle = 60, vjust=0.5)) + 
  ggtitle("Best send times, without day of send and weekend")
