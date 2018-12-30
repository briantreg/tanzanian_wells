library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggmap)
library(FNN)
library(stringdist)
library(OneR)

source("data_preparation/functions.R")

#Read in the data
predictors = 
    read.csv("data/4910797b-ee55-40a7-8668-10efd5c1b960.csv") %>% as.tibble()
response = 
    read.csv("data/0bf8bc6e-30d0-4c50-956a-603fc693d966.csv") %>% as.tibble()

#Join the predictors to response
prediction_set = 
    merge(response,
          predictors,
          by = "id") %>% 
    as.tibble()

#Set up a working set to isolate the original data
prediction_set_working = prediction_set

#Clean up unneeded data
rm(predictors, response)

#####Variable checking

#Status Group
table(prediction_set$status_group) %>%
    cbind("Observations:" = .)


#----------------Date_recorded, create year/month/weekday

month = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
weekday = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',  'Saturday', 'Sunday')

prediction_set_working$date_recorded_POSIX = as.POSIXct(prediction_set_working$date_recorded)
prediction_set_working$year_recorded = year(prediction_set_working$date_recorded_POSIX)
prediction_set_working$month_recorded = month(prediction_set_working$date_recorded_POSIX)  %>%
    month.abb[.] %>%
    factor(levels = month)
prediction_set_working$weekday_recorded = weekdays(prediction_set_working$date_recorded_POSIX) %>%
    factor(levels = weekday)
rm(month, weekday)

chart_statusfill_obs(prediction_set_working, "weekday_recorded", "Weekday")


##----------------Geographic


#------------Region
prediction_set_working = drop_col(prediction_set_working,'region_code')

table_char_freq(prediction_set_working, 
                'region',
                "Region") %>%
    t()


#------------District and LGA

prediction_set_working$district_region = as.factor(paste0(prediction_set_working$region,"_D",prediction_set_working$district_code))

district_region_count = table_char_freq(prediction_set_working, 
                                        'district_region',
                                        "District Region")
factor_summary_table(district_region_count)
lga_count = table_char_freq(prediction_set_working, 
                            'lga',
                            'LGA')
factor_summary_table(lga_count)
district_region_count$var = 'District Region'
lga_count$var = 'LGA'

districtr_lga_counts = rbind(lga_count[,c('Observations','var')],
                             district_region_count[,c('Observations','var')])

districtr_lga_counts$observations_ceiling100 = 
    data_buckets(districtr_lga_counts, "Observations", 100) 


title_name = paste0('Number Of LGAs and Districts By Observations Recorded')
y_name = paste0('LGAs and Districts')
x_name = paste0('Observations Recorded')


ggplot(districtr_lga_counts) +
    geom_bar(aes(x=observations_ceiling100, fill = var), position = 'dodge') + 
    theme_bw() +
    labs(x = x_name, title = title_name, y = y_name)+
    theme(panel.border = element_rect(fill = NA, colour = NA), 
          axis.line = element_line(colour = 'grey13'), 
          panel.grid.major = element_line(colour = NA), 
          panel.grid.minor = element_line(colour = NA))

rm(lga_count,
   district_region_count,
   districtr_lga_counts)

prediction_set_working = drop_col(prediction_set_working,'district_region')
prediction_set_working = drop_col(prediction_set_working,'district_code')

#------------Ward
prediction_set_working$ward_lga = paste0(prediction_set_working$ward,"_",prediction_set_working$lga)

prediction_set_working = drop_col(prediction_set_working, "ward")

ward_lga_count = table_char_freq(prediction_set_working, 
                             'ward_lga',
                             c("Ward Lga"))
factor_summary_table(ward_lga_count)

ward_lga_count$observations_ceiling10 = 
    data_buckets(ward_lga_count, "Observations", 10) 

chart_obs_agg(ward_lga_count, "observations_ceiling10", "Ward-LGA")

rm(ward_lga_count)

#------------Admin_Region

big_wards = table(prediction_set_working$ward_lga) %>%
    as.data.frame() %>%
    filter(Freq >= 150) %>%
    .$Var1 
big_ward_rows = prediction_set_working$ward_lga %in% big_wards


little_lgas = table(prediction_set_working$lga) %>%
    as.data.frame() %>%
    filter(Freq < 150) %>%
    .$Var1 
little_lga_rows = prediction_set_working$lga %in% little_lgas


prediction_set_working$admin_district = paste0(prediction_set_working$region,"_",prediction_set_working$lga,"_OTHER")
prediction_set_working$admin_district[big_ward_rows] = paste0(prediction_set_working$region[big_ward_rows],"_",prediction_set_working$lga[big_ward_rows],"_",prediction_set_working$ward[big_ward_rows])
prediction_set_working$admin_district[little_lga_rows] = paste0(prediction_set_working$region[little_lga_rows],"_OTHER_OTHER")

admin_district_count = table_char_freq(prediction_set_working, 
                                       'admin_district',
                                       c("Admin District"))

factor_summary_table(admin_district_count)

admin_district_count$observations_ceiling10 = 
    data_buckets(admin_district_count, "Observations", 100) 

chart_obs_agg(admin_district_count, "observations_ceiling10", "Admin District")

rm(admin_district_count, big_ward_rows, big_wards, little_lga_rows, little_lgas)

#--------IT WOULD BE INTERESTING TO SEE FUNCTIONALITY BY WARD SIZE

#--------------Subvillage

prediction_set_working$subvillage = tolower(prediction_set_working$subvillage)
prediction_set_working$subvillage = gsub("[^[:alpha:] ]","",prediction_set_working$subvillage)
prediction_set_working$subvillage = gsub('\\b\\w{1}\\b','',prediction_set_working$subvillage)
prediction_set_working$subvillage = gsub("  "," ",prediction_set_working$subvillage)
prediction_set_working$subvillage = trimws(prediction_set_working$subvillage)

prediction_set_working$subvillage[prediction_set_working$subvillage == ''] = NA

prediction_set_working$subvillage_ward = paste0(prediction_set_working$subvillage, "_", prediction_set_working$ward_lga)

prediction_set_working$subvillage_ward[is.na(prediction_set_working$subvillage)] = NA

subvillage_ward_count = table_char_freq(prediction_set_working, 
                                       'subvillage_ward',
                                       c("Admin District"))

factor_summary_table(subvillage_ward_count)

subvillage_ward_count$observations_ceiling1 = 
    data_buckets(subvillage_ward_count, "Observations", 1) 

chart_obs_agg(subvillage_ward_count, "observations_ceiling1", "Subvillage-Ward")

prediction_set_working = drop_col(prediction_set_working, "subvillage")

rm(subvillage_ward_count)

####-----------GPS Data

#---------------Longitude/Latitude

chart_density(prediction_set_working, "longitude", "Longitude")

prediction_set_nolon = prediction_set_working[prediction_set_working$longitude == 0,]
prediction_set_lon = prediction_set_working[!prediction_set_working$longitude == 0,]

prediction_set_nolon$latitude_best = NA %>%
    as.numeric()
prediction_set_nolon$longitude_best = NA %>%
    as.numeric()

prediction_set_nolon = find_new_latlon(prediction_set_nolon, prediction_set_lon, "subvillage_ward")
sum(!is.na(prediction_set_nolon$longitude_new)) - sum(!is.na(prediction_set_nolon$longitude_best))
new_latlon()
prediction_set_nolon = drop_col(prediction_set_nolon, "longitude_new")
prediction_set_nolon = drop_col(prediction_set_nolon, "latitude_new")

prediction_set_nolon = find_new_latlon(prediction_set_nolon, prediction_set_lon, "ward_lga")
sum(!is.na(prediction_set_nolon$longitude_new)) - sum(!is.na(prediction_set_nolon$longitude_best))
new_latlon()
prediction_set_nolon = drop_col(prediction_set_nolon, "longitude_new")
prediction_set_nolon = drop_col(prediction_set_nolon, "latitude_new")

prediction_set_nolon = find_new_latlon(prediction_set_nolon, prediction_set_lon, "lga")
sum(!is.na(prediction_set_nolon$longitude_new)) - sum(!is.na(prediction_set_nolon$longitude_best))
new_latlon()
prediction_set_nolon = drop_col(prediction_set_nolon, "longitude_new")
prediction_set_nolon = drop_col(prediction_set_nolon, "latitude_new")


prediction_set_nolon = find_new_latlon(prediction_set_nolon, prediction_set_lon, "region")
sum(!is.na(prediction_set_nolon$longitude_new)) - sum(!is.na(prediction_set_nolon$longitude_best))
new_latlon()
prediction_set_nolon = drop_col(prediction_set_nolon, "longitude_new")
prediction_set_nolon = drop_col(prediction_set_nolon, "latitude_new")

prediction_set_nolon$latitude = prediction_set_nolon$latitude_best
prediction_set_nolon$longitude = prediction_set_nolon$longitude_best

prediction_set_working = prediction_set_nolon %>%
    select(-contains("best")) %>%
    rbind(prediction_set_lon)

tanzania_map = get_map(location = "tanzania", maptype = "roadmap",zoom = 6)
?get_map()
ggmap(tanzania_map, extent = "device") + geom_point(aes(x = longitude, y = latitude), colour = "red", 
                                                    alpha = 0.05, size = 1, data = prediction_set_nolon)
ggmap(tanzania_map, extent = "device") + geom_point(aes(x = longitude, y = latitude), colour = "blue", 
                                                    alpha = 0.05, size = 0.2, data = prediction_set_working)

rm(prediction_set_nolon, prediction_set_lon, tanzania_map)

#---------------Altitude

chart_density(prediction_set_working, "gps_height")

## AltitudeLookupExperiment

set.seed(80085)
nonzero_height_sample = sample_n(
    prediction_set_working[!prediction_set_working$gps_height == 0,c("gps_height","latitude","longitude")],5)

nonzero_height_sample = nonzero_height_sample %>%
    cbind(online_height = c(2213,
                            1010,
                            1721,
                            1436,
                            224)
    )
nonzero_height_sample$height_diff = nonzero_height_sample$gps_height - nonzero_height_sample$online_height

zero_height_sample = sample_n(
    prediction_set_working[prediction_set_working$gps_height == 0,c("id","gps_height","latitude","longitude")],5)

zero_height_sample = zero_height_sample %>%
    cbind(online_height = c(927,
                            1052,
                            1209,
                            1291,
                            22)
    )

zero_height_sample
nonzero_height_sample

prediction_set_working = prediction_set_working %>%
    rename(altitude_metres = gps_height)

## AltitudeKNN


altitude_knn_set = prediction_set_working[prediction_set_working$latitude != 0 & prediction_set_working$altitude_metres != 0,c("id","altitude_metres","longitude","latitude")]

altitude_knn_set_test = sample_n(altitude_knn_set,nrow(altitude_knn_set)/10)
altitude_knn_set_train = altitude_knn_set[!altitude_knn_set$id %in% altitude_knn_set_test$id,]

set.seed(80085)


for(i in 1:7) {
    altitude_knn_model = knn.reg(
        train = altitude_knn_set_train[,c("longitude","latitude")], 
        test = altitude_knn_set_test[,c("longitude","latitude")],
        y =altitude_knn_set_train$altitude_metres,
        k = i
    )
    MSE = mean((altitude_knn_set_test$altitude_metres - altitude_knn_model$pred)^2)
    K = altitude_knn_model$k
    if(!exists("knn_performance")) {
        knn_performance = 
            data.frame(K, MSE)
    } else {
        knn_performance = knn_performance %>%
            rbind(c(K, MSE))
    }
}

best_K = knn_performance$K[knn_performance$MSE == min(knn_performance$MSE)]

prediction_set_working_nonalti = prediction_set_working[prediction_set_working$altitude_metres == 0,]
prediction_set_working_alti = prediction_set_working[!prediction_set_working$altitude_metres == 0,]

altitude_best_knn_model = knn.reg(
    train = prediction_set_working_alti[,c("longitude","latitude")], 
    test = prediction_set_working_nonalti[,c("longitude","latitude")],
    y =prediction_set_working_alti$altitude_metres,
    k = best_K
)


prediction_set_working_nonalti$altitude_metres = altitude_best_knn_model$pred

set.seed(80085)
new_height_sample = sample_n(
    prediction_set_working_nonalti[,c("altitude_metres","latitude","longitude")],5)

new_height_sample = new_height_sample %>%
    cbind(online_height = c(1297,
                            502,
                            1430,
                            1354,
                            1309)
    )
new_height_sample$height_diff = new_height_sample$altitude_metres - new_height_sample$online_height
prediction_set_working = rbind(prediction_set_working_nonalti, prediction_set_working_alti)

rm(prediction_set_working_nonalti, prediction_set_working_alti,
   altitude_best_knn_model,new_height_sample,
   altitude_knn_set_test, altitude_knn_set_train, altitude_knn_set,
   knn_performance, nonzero_height_sample, zero_height_sample,
   best_K, i, K, MSE, altitude_knn_model)

####-----------Well Construction

#---------------Installer

prediction_set_working$installer = char_clean(prediction_set_working, "installer")

installer_count = table_char_freq(prediction_set_working, 
                                        'installer',
                                        c("Well Installer"))

factor_summary_table(installer_count)

prediction_set_working$installer_water_engineer = 0
prediction_set_working[prediction_set_working$installer %in% c("dwe","rwe"),"installer_water_engineer"] = 1


table(prediction_set_working[,c("status_group","installer_water_engineer")]) %>%
    prop.table(2) %>%
    {. * 100} %>% 
    round(1)

rm(installer_count)
#---------------Funder

prediction_set_working$funder = char_clean(prediction_set_working, "funder")

funder_count = table_char_freq(prediction_set_working, 
                                  'funder',
                                  c("Well funder"))

factor_summary_table(funder_count)

###GovtFunde
prediction_set_working$government_funded  = 0
prediction_set_working$government_funded[prediction_set_working$funder %in% c("government of tanzania")] = 1


table(prediction_set_working[,c("status_group","government_funded")]) %>%
    prop.table(2) %>%
    {. * 100} %>% 
    round(1)

###SameFunder/installer

prediction_set_working$installer[prediction_set_working$installer == ''] = 'unknown_installer'
prediction_set_working$installer[prediction_set_working$funder == ''] = 'unknown_funder'

funder_installer_similarity = stringdist(prediction_set_working$funder, 
                                         prediction_set_working$installer,
                                         method = "osa")

prediction_set_working$funder_installer = 0

prediction_set_working$funder_installer[funder_installer_similarity <= 2] = 1


table(prediction_set_working[,c("status_group","funder_installer")]) %>%
    prop.table(2) %>%
    {. * 100} %>% 
    round(1)

prediction_set_working = drop_col(prediction_set_working, "installer")
prediction_set_working = drop_col(prediction_set_working, "funder")

rm(funder_count,funder_installer_similarity)

#-------construction year

prediction_set_working$construction_year = as.numeric(prediction_set_working$construction_year)
prediction_set_working$construction_year[prediction_set_working$construction_year == 0] = NA
chart_obs(prediction_set_working,"construction_year","Construction Year")
chart_percent(prediction_set_working,"construction_year")

prediction_set_working$construction_decade = 'None'
prediction_set_working$construction_decade[which(!is.na(prediction_set_working$construction_year))] = 
    paste0(substr(prediction_set_working$construction_year[which(!is.na(prediction_set_working$construction_year))],1,3),"0s")

chart_percent(prediction_set_working,"construction_decade")

prediction_set_working = drop_col(prediction_set_working, "construction_year")


#-------------Total Static Head
summary(prediction_set_working$amount_tsh)

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

chart_percent(prediction_set_working,"total_static_head_grp", "Total Static Head Group")
#--------Basin
table_char_freq(prediction_set_working,"basin", "Water Basin")
chart_percent(prediction_set_working,"basin")

#--------Water Source

chart_percent(prediction_set_working,"source")
prediction_set_working = drop_col(prediction_set_working, "source_type")

chart_percent(prediction_set_working,"source_class")

#-------Quantity
prediction_set_working = drop_col(prediction_set_working, "quantity_group")
chart_percent(prediction_set_working,"quantity")

#----------Quality

prediction_set_working$abandoned_quality = 'other'
prediction_set_working$abandoned_quality[grep("abandoned", prediction_set_working$water_quality)] = 'abandoned'

prediction_set_working = drop_col(prediction_set_working, "water_quality")

chart_percent(prediction_set_working,"quality_group")
chart_percent(prediction_set_working,"abandoned_quality")

#----------Extraction Method

chart_data = ggplot(prediction_set_working[
    prediction_set_working$extraction_type_class %in% c("handpump","motorpump"),])

chart_data+
    geom_bar(aes(x = extraction_type_group, y= id, fill = status_group),position = "fill", stat = "identity") + 
    scale_y_continuous() +
    facet_grid(~extraction_type_class, 
               scales = "free_x")+ 
    scale_fill_manual(values = c("firebrick","orangered","forestgreen"))

prediction_set_working = drop_col(prediction_set_working, "extraction_type_class")
prediction_set_working = drop_col(prediction_set_working, "extraction_type")
rm(chart_data)
#-------------Water point type

prediction_set_working = drop_col(prediction_set_working, "waterpoint_type")
prediction_set_working = drop_col(prediction_set_working, "waterpoint_type_group")

#-------------Well Scheme

#Managment

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

scheme_managment_broad

prediction_set_working = merge(prediction_set_working,
                               scheme_managment_broad,
                               by = "scheme_management",
                               all.x = TRUE)

prediction_set_working$scheme_management_broad = prediction_set_working$scheme_management_broad %>%
    as.factor()

chart_percent(prediction_set_working,"scheme_management_broad")

#Name

prediction_set_working$scheme_name = char_clean(prediction_set_working, "scheme_name")

scheme_count = table_char_freq(prediction_set_working, 
                                  'scheme_name',
                                  c("Well Installer"))

prediction_set_working = drop_col(prediction_set_working, "scheme_name")

rm(scheme_count, scheme_managment_broad)

#--------Well Management

chart_percent(prediction_set_working,"management_group")

#-------------Payment
prediction_set_working = drop_col(prediction_set_working, "payment_type")
chart_percent(prediction_set_working,"payment")

#--------------Permit

prediction_set_working$permit = prediction_set_working$permit %>%
    as.character()
prediction_set_working$permit[prediction_set_working$permit == ''] = 'unknown'
prediction_set_working$permit = prediction_set_working$permit %>%
    as.factor()
chart_percent(prediction_set_working,"permit")

#--------------Public Meeting
prediction_set_working$public_meeting = prediction_set_working$public_meeting %>%
    as.character()
prediction_set_working$public_meeting[prediction_set_working$public_meeting == ''] = 'unknown'
prediction_set_working$public_meeting = prediction_set_working$public_meeting %>%
    as.factor()
chart_percent(prediction_set_working,"public_meeting")

#-------------Population
summary(prediction_set_working$population)

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

chart_percent(prediction_set_working,"population_bins")


#--------Drop variables
prediction_set_working = drop_col(prediction_set_working, "num_private")
prediction_set_working = drop_col(prediction_set_working, "wpt_name")
prediction_set_working = drop_col(prediction_set_working, "recorded_by")
