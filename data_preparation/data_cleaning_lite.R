setRepositories(ind = c(1:6, 8))

#Libraries####
da_packages = c(
    'tidyverse', 
    'lubridate', 
    'FNN', 
    'stringdist', 
    'OneR',
    'mlr', 
    'clue',
    'clusterSim')

cond_inst_package = function(p) {
    if(!p %in% rownames(installed.packages())) {
    install.packages(p)
    }
} 
install.packages('mlr')
library(mlr)
install.packages('clue')
library(clue)
install.packages('genefilter')
library(genefilter)
install.packages('clusterSim')
library(clusterSim)

####Read in the data and custom functions####
predictors = 
    read.csv("data/4910797b-ee55-40a7-8668-10efd5c1b960.csv") %>% as_tibble()
response = 
    read.csv("data/0bf8bc6e-30d0-4c50-956a-603fc693d966.csv") %>% as_tibble()
source('data_preparation/functions.R')

#####Join the predictors to response####
prediction_set_working = 
    merge(response,
          predictors,
          by = "id") %>% 
    as.tibble()

#####Add weekday transformation####
weekday = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',  'Saturday', 'Sunday')
prediction_set_working$date_recorded_POSIX = as.POSIXct(prediction_set_working$date_recorded)
prediction_set_working$weekday_recorded = weekdays(prediction_set_working$date_recorded_POSIX) %>%
    factor(levels = weekday)

####Combine ward with LGA####
prediction_set_working$ward_lga = paste0(prediction_set_working$ward,"_",prediction_set_working$lga)

####Clean up and combine subvillage####
prediction_set_working$subvillage = tolower(prediction_set_working$subvillage)
prediction_set_working$subvillage = gsub("[^[:alpha:] ]","",prediction_set_working$subvillage)
prediction_set_working$subvillage = gsub('\\b\\w{1}\\b','',prediction_set_working$subvillage)
prediction_set_working$subvillage = gsub("  "," ",prediction_set_working$subvillage)
prediction_set_working$subvillage = trimws(prediction_set_working$subvillage)

prediction_set_working$subvillage[prediction_set_working$subvillage == ''] = NA

prediction_set_working$subvillage_ward_lga = paste0(prediction_set_working$subvillage, "_", prediction_set_working$ward_lga)

prediction_set_working$subvillage_ward_lga[is.na(prediction_set_working$subvillage)] = NA


####Create admin region####
prediction_set_working$admin_district = paste0(prediction_set_working$region,"_",prediction_set_working$lga,"_OTHER")

little_lgas = table(prediction_set_working$lga) %>%
    as.data.frame() %>%
    filter(Freq < 50) %>%
    .$Var1 
little_lga_rows = prediction_set_working$lga %in% little_lgas
prediction_set_working$admin_district[little_lga_rows] = 
    paste0(prediction_set_working$region[little_lga_rows],"_OTHER_OTHER")

big_wards = table(prediction_set_working$ward_lga) %>%
    as.data.frame() %>%
    filter(Freq >= 50) %>%
    .$Var1 
big_ward_rows = prediction_set_working$ward_lga %in% big_wards
prediction_set_working$admin_district[big_ward_rows] = 
    paste0(prediction_set_working$region[big_ward_rows],"_",
           prediction_set_working$lga[big_ward_rows],"_",
           prediction_set_working$ward[big_ward_rows])

####Weight of Information transformation for admin_district####

admin_district_woe = 
    as.data.frame.matrix(
        table(prediction_set_working$admin_district,prediction_set_working$status_group)
    )

admin_district_woe = admin_district_woe %>%
    setNames(c("functional","functional_repair","non_functional"))
functional_count = sum(admin_district_woe$functional)
nonfunctional_count = sum(admin_district_woe$non_functional)
repairfunctional_count = sum(admin_district_woe$functional_repair)

admin_district_woe$woe_region_functional = log(
    ( (admin_district_woe$functional + 0.0001) / functional_count ) /
        (
            (admin_district_woe$functional_repair + admin_district_woe$non_functional + 0.0001 ) / 
                (nonfunctional_count + repairfunctional_count)
        ) 
) %>%
    round(5)


admin_district_woe$woe_region_repair = log (
    
    ( (admin_district_woe$functional_repair + 0.0001) /  repairfunctional_count) /
        (
            (admin_district_woe$functional + admin_district_woe$non_functional + 0.0001 ) / 
                (nonfunctional_count + functional_count)
        )
)    %>%
    round(5)

admin_district_woe$woe_region_nonfunctional = log (
    
    ( (admin_district_woe$non_functional + 0.0001) / nonfunctional_count ) /
        (
            (admin_district_woe$functional + admin_district_woe$functional_repair + 0.0001 ) / 
                (repairfunctional_count + functional_count)
        )
)    %>%
    round(5)

admin_district_woe$admin_district = row.names(admin_district_woe)
rownames(admin_district_woe) = 1:nrow(admin_district_woe)

saveRDS(admin_district_woe, "admin_district_woe.RDS")
prediction_set_working = merge(prediction_set_working,
                               admin_district_woe[,c("admin_district","woe_region_functional","woe_region_repair","woe_region_nonfunctional")],
                               by = "admin_district",
                               all.x = TRUE)

####Height/Alt Clustering####

prediction_set_working = prediction_set_working %>%
    rename(altitude_metres = gps_height)

prediction_set_complete = prediction_set_working[
    !prediction_set_working$longitude == 0 &
        !prediction_set_working$altitude_metres == 0,]

prediction_set_complete$altitude_metres_log = log(prediction_set_complete$altitude_metres + 0.01)
summary(prediction_set_complete$altitude_metres)

prediction_set_complete[,c("longitude",
                           "latitude",
                           "altitude_metres")]

geo_cluster_vars = c("longitude",
                     "latitude"#,
#                     "altitude_metres"
)

geoClstr_Task = makeClusterTask(data = prediction_set_complete[,geo_cluster_vars])

geoClstr_param <- makeParamSet(
    makeDiscreteParam('centers', values = c(2:20))
)
geoClstr_resample = makeResampleDesc('CV', iters = 3)
geoClstr_ctrl = makeTuneControlGrid()
geoClstr_tune = tuneParams('cluster.kmeans',
                         task = geoClstr_Task,
                         resampling = geoClstr_resample,
                         par.set = geoClstr_param,
                         control = geoClstr_ctrl,
                         measures = list(db, G1))

generateHyperParsEffectData(geoClstr_tune)

geoClstr_learner = setHyperPars(makeLearner('cluster.kmeans'), 
                               par.vals = geoClstr_tune$x)

geoCluster_model = train(task = geoClstr_Task, learner = geoClstr_learner)

geoCluster = list(model = geoCluster_model,
              vars = geo_cluster_vars)

saveRDS(geoCluster, 'geoCluster.RDS')

####Longitude/Latitude imputing missing values####

#Separate missing and non-missing lat lon records 
prediction_set_nolon = prediction_set_working[prediction_set_working$longitude == 0,]
prediction_set_lon = prediction_set_working[!prediction_set_working$longitude == 0,]

#Create new lat/lon variables to capture the best lat lon estimates
prediction_set_nolon$latitude_best = NA %>%
    as.numeric()
prediction_set_nolon$longitude_best = NA %>%
    as.numeric()

lat_lon_impute = prediction_set_lon[,c("latitude","longitude","subvillage_ward_lga","ward_lga","lga","region")]
saveRDS(lat_lon_impute, "lat_lon_impute.RDS")

#Set missing latlon to  the mean latlon of the subvillage, if available
prediction_set_nolon = find_new_latlon(prediction_set_nolon, prediction_set_lon, "subvillage_ward_lga")
new_latlon()
prediction_set_nolon = drop_col(prediction_set_nolon, "longitude_new")
prediction_set_nolon = drop_col(prediction_set_nolon, "latitude_new")
#Update still missing to the mean latlon of the ward, if available
prediction_set_nolon = find_new_latlon(prediction_set_nolon, prediction_set_lon, "ward_lga")
new_latlon()
prediction_set_nolon = drop_col(prediction_set_nolon, "longitude_new")
prediction_set_nolon = drop_col(prediction_set_nolon, "latitude_new")
#Update still missing latlon to the mean latlon of the lga, if available
prediction_set_nolon = find_new_latlon(prediction_set_nolon, prediction_set_lon, "lga")
new_latlon()
prediction_set_nolon = drop_col(prediction_set_nolon, "longitude_new")
prediction_set_nolon = drop_col(prediction_set_nolon, "latitude_new")
#Update still missing latlon to the mean latlon of the region
prediction_set_nolon = find_new_latlon(prediction_set_nolon, prediction_set_lon, "region")
new_latlon()
prediction_set_nolon = drop_col(prediction_set_nolon, "longitude_new")
prediction_set_nolon = drop_col(prediction_set_nolon, "latitude_new")
#Replace lat lon with the best ones found in this process
prediction_set_nolon$latitude = prediction_set_nolon$latitude_best
prediction_set_nolon$longitude = prediction_set_nolon$longitude_best
#Drop new "best" lat lon cols
prediction_set_working = prediction_set_nolon %>%
    select(-contains("best")) %>%
    rbind(prediction_set_lon)

#####Altitude####
set.seed(80085)

#Get data where latitude is not 0 and altitude is 0, for the KNN
altitude_knn_set = prediction_set_working[prediction_set_working$latitude != 0 & prediction_set_working$altitude_metres != 0,c("id","altitude_metres","longitude","latitude")]

#Test and train set for KNN
altitude_knn_set_test = sample_n(altitude_knn_set,nrow(altitude_knn_set)/10)
altitude_knn_set_train = altitude_knn_set[!altitude_knn_set$id %in% altitude_knn_set_test$id,]

alt_pred_vars = c("id","altitude_metres","longitude","latitude")

AltKNN_task <- makeRegrTask(data = altitude_knn_set_train[,alt_pred_vars], target = 'altitude_metres')
AltKNN_test <- makeRegrTask(data = altitude_knn_set_test[,alt_pred_vars], target = 'altitude_metres')
AltKNN_param <- makeParamSet(
    makeDiscreteParam('k', values = c(1,2,3,4,5,6,7)),
    makeDiscreteParam('distance', values = c(0.5,1,2,4))
)
AltKNN_resample = makeResampleDesc('CV', iters = 5)
AltKNN_ctrl = makeTuneControlGrid()
AltKNN_tune = tuneParams('regr.kknn',
                         task = AltKNN_task,
                         resampling = AltKNN_resample,
                         par.set = AltKNN_param,
                         control = AltKNN_ctrl)


AltKNN_learner <- setHyperPars(makeLearner('regr.kknn'), 
                               par.vals = AltKNN_tune$x)

resample(AltKNN_learner, AltKNN_task, AltKNN_resample)

AltKNN_model <- train(AltKNN_learner, AltKNN_task)

AltKNN = list(model = AltKNN_model,
     vars = alt_pred_vars)
saveRDS(AltKNN_model, "AltKNN_model.rds")

preds = predict(AltKNN_model, AltKNN_test)

prediction_set_working_nonalti = prediction_set_working[prediction_set_working$altitude_metres == 0,]

AltKNN_pred <- makeRegrTask(data = prediction_set_working_nonalti[,alt_pred_vars], 
                            target = 'altitude_metres')
new_preds = predict(AltKNN_model, AltKNN_pred)

prediction_set_working_nonalti$altitude_metres = round(new_preds$data$response)

prediction_set_working_alti = prediction_set_working[!prediction_set_working$altitude_metres == 0,]

prediction_set_working = rbind(
    prediction_set_working_alti,
    prediction_set_working_nonalti)

####Find clusters for all data####

geo_data = prediction_set_working[,geo_cluster_vars]

cluster_pred = predict(geoCluster_model, newdata = geo_data)

prediction_set_working$geo_cluster = cluster_pred$data$response

#####Installer####

prediction_set_working$installer = char_clean(prediction_set_working, "installer")
#WaterEngineer Installed
prediction_set_working$installer_water_engineer = 0
prediction_set_working[prediction_set_working$installer %in% c("dwe","rwe"),"installer_water_engineer"] = 1

#####Funder####
prediction_set_working$funder = char_clean(prediction_set_working, "funder")
#GovtFunded
prediction_set_working$government_funded  = 0
prediction_set_working$government_funded[prediction_set_working$funder %in% c("government of tanzania")] = 1

#####SameFunder/installer####


prediction_set_working$installer[prediction_set_working$installer == ''] = 'unknown_installer'
prediction_set_working$installer[prediction_set_working$funder == ''] = 'unknown_funder'

funder_installer_similarity = stringdist(prediction_set_working$funder, 
                                         prediction_set_working$installer,
                                         method = "osa")

prediction_set_working$funder_installer = 0

prediction_set_working$funder_installer[funder_installer_similarity <= 2] = 1

#####construction year####

prediction_set_working$construction_year = as.numeric(prediction_set_working$construction_year)
prediction_set_working$construction_year[prediction_set_working$construction_year == 0] = NA

#####Construction decade####
prediction_set_working$construction_decade = 'None'
prediction_set_working$construction_decade[which(!is.na(prediction_set_working$construction_year))] = 
    paste0(substr(prediction_set_working$construction_year[which(!is.na(prediction_set_working$construction_year))],1,3),"0s")
prediction_set_working$construction_decade = as.factor(prediction_set_working$construction_decade)

#####Grouping Total Static Head####

prediction_set_working$total_static_head_grp = NA
prediction_set_working$total_static_head_grp[prediction_set_working$amount_tsh == 0]  = 'Zero'

prediction_set_working$total_static_head_grp[prediction_set_working$amount_tsh != 0] = 
    bin(prediction_set_working$amount_tsh[prediction_set_working$amount_tsh != 0],
        5,
        method="content") %>%
    as.character()

prediction_set_working$total_static_head_grp  = prediction_set_working$total_static_head_grp %>%
    as.factor()
levels(prediction_set_working$total_static_head_grp) = c('1-30', '100-500', '30-100', '1000+', '500-1000', 'Zero')

prediction_set_working$total_static_head_grp = prediction_set_working$total_static_head_grp %>%
    as.character() %>%
    factor(c('Zero', '1-30', '30-100', '100-500', '500-1000', '1000+'))

#####Quality - Abandoned Flag####

prediction_set_working$abandoned_quality = 'other'
prediction_set_working$abandoned_quality[grep("abandoned", prediction_set_working$water_quality)] = 'abandoned'
prediction_set_working$abandoned_quality = as.factor(prediction_set_working$abandoned_quality)

####Create Broad Scheme Managment####

prediction_set_working$scheme_management[prediction_set_working$scheme_management %in% c('None','')] = 'Other'

scheme_managment_broad = rbind(
    c('Other','Other'),
    c('Company','Private'),
    c('Private operator','Private'),
    c('Trust','Community'),
    c('SWC','Community'),
    c('VWC','Community'),
    c('WUA','Community'),
    c('WUG','Community'),
    c('Parastatal','Government'),
    c('Water authority','Government'),
    c('Water Board','Government')) %>%
    as.data.frame() %>%
    setNames(c("scheme_management","scheme_management_broad"))

prediction_set_working = merge(prediction_set_working,
                               scheme_managment_broad,
                               by = "scheme_management",
                               all.x = TRUE)

prediction_set_working$scheme_management_broad = prediction_set_working$scheme_management_broad %>%
    as.factor()

#####Permit####

prediction_set_working$permit = prediction_set_working$permit %>%
    as.character()
prediction_set_working$permit[prediction_set_working$permit == ''] = 'unknown'
prediction_set_working$permit = prediction_set_working$permit %>%
    as.factor()

#####Public Meeting####
prediction_set_working$public_meeting = prediction_set_working$public_meeting %>%
    as.character()
prediction_set_working$public_meeting[prediction_set_working$public_meeting == ''] = 'unknown'
prediction_set_working$public_meeting = prediction_set_working$public_meeting %>%
    as.factor()

#####Binned Population####
prediction_set_working$population[prediction_set_working$population == 0]  = NA
prediction_set_working$population_bins = 'unknown'
prediction_set_working$population_bins[which(!is.na(prediction_set_working$population))] = 
    bin(prediction_set_working$population[which(!is.na(prediction_set_working$population))],
        5,
        method="content") %>%
    as.character()

prediction_set_working$population_bins  = prediction_set_working$population_bins %>%
    as.factor()
levels(prediction_set_working$population_bins) = c('0-18', '100-203', '18-100', '203-400', '400+', 'unknown')

prediction_set_working$population_bins = prediction_set_working$population_bins %>%
    as.character() %>%
    factor(c('0-18', '18-100', '100-203', '203-400', '400+', 'unknown'))
