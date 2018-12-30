


prediction_set_working = prediction_set



#----------------Date_recorded

month = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
weekday = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',  'Saturday', 'Sunday')

prediction_set_working$date_recorded_POSIX = as.POSIXct(prediction_set_working$date_recorded)
prediction_set_working$year_recorded = year(prediction_set_working$date_recorded_POSIX)
prediction_set_working$month_recorded = month(prediction_set_working$date_recorded_POSIX)  %>%
    month.abb[.] %>%
    factor(levels = month)
prediction_set_working$weekday_recorded = weekdays(prediction_set_working$date_recorded_POSIX) %>%
    factor(levels = weekday)

chart_status_obs_facetrow(prediction_set_working, "month_recorded", 'year_recorded ~ .')
chart_statusfill_obs(prediction_set_working, "weekday_recorded")


##----------------Geographic

length(unique(prediction_set_working$district_code))
length(levels(prediction_set_working$lga))
length(levels(prediction_set_working$ward))
length(levels(prediction_set_working$subvillage))


#------------Region
prediction_set_working = drop_col(prediction_set_working,'region_code')

table_char_freq(prediction_set_working, 
                'region',
                c("Region","Observations")) %>%
    t()

#------------District and LGA

prediction_set_working$district_region = as.factor(paste0(prediction_set_working$region,"_D",prediction_set_working$district_code))

district_region_count = table_char_freq(prediction_set_working, 
                'district_region',
                c("district_region","Observations"))

district_region_count$observations_floor100 = ceiling(district_region_count$Observations/100)*100

district_region_count$var = 'District Region'

lga_count = table_char_freq(prediction_set_working, 
                                        'lga',
                                        c("lga","Observations"))

lga_count$observations_floor100 = ceiling(lga_count$Observations/100)*100

lga_count$var = 'LGA'

districtr_lga_counts = rbind(lga_count[,c('observations_floor100','var')],
      district_region_count[,c('observations_floor100','var')])

ggplot(districtr_lga_counts) +
    geom_col(aes(x=observations_floor100, group  = var))


table(prediction_set_working$district_code)
length(unique(prediction_set_working$district_code))
length(levels(prediction_set_working$lga))
length(levels(prediction_set_working$ward))
length(levels(prediction_set_working$subvillage))



chart_data = ggplot(prediction_set_working)

chart_data +
    geom_bar(aes(x = region))


#-----------------------------------------
    
district_region_count = aggregate(id ~ district_region, prediction_set, length) %>%
    rename(observations = id)

summary(district_region_count$observations)


district_region_count$observations_floor100 = ceiling(district_region_count$observations/100)*100

chart_data = ggplot(district_region_count)

chart_data +
    geom_bar(aes(x = observations_floor100)) +
    theme_bw()

numer = length(levels(prediction_set_working$district_region))
denom = sum(district_region_count$observations_floor100 <= 500)
numer
denom/numer

#------------------

lga_count = aggregate(id ~ lga, prediction_set, length) %>%
    rename(observations = id)

nrow(lga_count)

summary(lga_count$observations)

lga_count$observations_floor100 = ceiling(lga_count$observations/100)*100
    
chart_data = ggplot(lga_count)

chart_data +
    geom_bar(aes(x = observations_floor100)) +
    theme_bw()

table(prediction_set_working$district_region,prediction_set_working$lga) %>%
    as.data.frame() %>%
    filter(Freq > 0) %>%
    arrange(Var1, Var2, Freq)

#------------------


ward_count = aggregate(id ~ ward, prediction_set, length) %>%
    rename(observations = id)

summary(ward_count$observations)

nrow(ward_count)

ward_count$observations_floor10 = floor(ward_count$observations/10)*10

chart_data = ggplot(ward_count)

chart_data +
    geom_bar(aes(x = observations_floor10))

table(prediction_set_working$ward,prediction_set_working$lga) %>%
    as.data.frame() %>%
    filter(Freq > 0) %>%
    .$Var1 %>%
    table() %>%
    as.data.frame() %>%
    filter(Freq > 1) %>%
    nrow()

prediction_set_working$ward_lga = paste0(prediction_set_working$ward,"_",prediction_set_working$lga)

#------------------

ward_lga_count = aggregate(id ~ ward_lga, prediction_set, length) %>%
    rename(observations = id)

summary(ward_lga_count$observations)

nrow(ward_lga_count)

ward_lga_count$observations_floor10 = floor(ward_lga_count$observations/10)*10

chart_data = ggplot(ward_lga_count)

chart_data +
    geom_bar(aes(x = observations_floor10))

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

prediction_set_working[big_ward_rows,c("admin_district","region","lga","ward_lga")]
#------------------

admin_district_count = aggregate(id ~ admin_district, prediction_set, length) %>%
    rename(observations = id)

nrow(admin_district_count)

summary(admin_district_count$observations)

admin_district_count$observations_floor100 = ceiling(admin_district_count$observations/100)*100

chart_data = ggplot(admin_district_count)

chart_data +
    geom_bar(aes(x = observations_floor100)) +
    theme_bw()

table(prediction_set_working$admin_district) %>%
    as.data.frame() %>%
    filter(Freq > 0) %>%
    arrange(Freq, Var1)
#------------------

subvillage_count = aggregate(id ~ subvillage, prediction_set, length) %>%
    rename(observations = id)
summary(subvillage_count$observations)
nrow(subvillage_count)

prediction_set_working$subvillage = gsub("[^[:alnum:] ]","",prediction_set_working$subvillage) %>%
    as.factor()
prediction_set_working$subvillage = trimws(prediction_set_working$subvillage) %>%
    as.factor()

na_subvillages = levels(prediction_set_working$subvillage)[grep("[[:digit:]]+", levels(prediction_set_working$subvillage))] %>%
    c(levels(prediction_set_working$subvillage2)[nchar(levels(prediction_set_working$subvillage2)) <= 2])

na_subvillage_rows = prediction_set_working$subvillage %in% na_subvillages

prediction_set_working$subvillage[na_subvillage_rows] = NA

prediction_set_working$subvillage_ward = paste0(prediction_set_working$subvillage, "_", prediction_set_working$ward)
prediction_set_working$subvillage_ward[na_subvillage_rows] = NA

#------------------

subvillage_ward_count = aggregate(id ~ subvillage_ward, prediction_set, length) %>%
    rename(observations = id)

nrow(subvillage_ward_count)

summary(subvillage_ward_count$observations)

nrow(subvillage_ward_count[subvillage_ward_count$observations == 1,])
nrow(subvillage_ward_count[subvillage_ward_count$observations > 10,])

subvillage_ward_count$observations_floor100 = ceiling(subvillage_ward_count$observations/100)*100

chart_data = ggplot(subvillage_ward_count)

chart_data +
    geom_bar(aes(x = observations_floor100)) +
    theme_bw()

table(prediction_set_working$subvillage_ward) %>%
    as.data.frame() %>%
    filter(Freq > 0) %>%
    arrange(Freq, Var1)
#------------------

#GPS

summary(prediction_set_working$longitude)

chart_data = ggplot(prediction_set_working)

chart_data +
    geom_density(aes(x = longitude))


prediction_set_nolon = prediction_set_working[prediction_set_working$longitude == 0,]
prediction_set_lon = prediction_set[!prediction_set_working$longitude == 0,]
nrow(prediction_set_nolon)

#Create subvillage averages for lat lon
subvillage_ward_means = prediction_set_lon %>%
    group_by(subvillage_ward) %>%
    summarise(longitude_subvillage_ward_mean = mean(longitude))

subvillage_ward_means = prediction_set_lon %>%
    group_by(subvillage_ward) %>%
    summarise(latitude_subvillage_ward_mean = mean(latitude)) %>%
    merge(subvillage_ward_means,
          by = "subvillage_ward")

prediction_set_nolon = merge(prediction_set_nolon,
      subvillage_ward_means,
      by = "subvillage_ward",
      all.x = TRUE)

summary(prediction_set_nolon$longitude_subvillage_ward_mean)
summary(prediction_set_nolon$latitude_subvillage_ward_mean)

#Create ward averages for lat lon

ward_lga_means = prediction_set_lon %>%
    group_by(ward_lga) %>%
    summarise(longitude_ward_lga_mean = mean(longitude))

ward_lga_means = prediction_set_lon %>%
    group_by(ward_lga) %>%
    summarise(latitude_ward_lga_mean = mean(latitude)) %>%
    merge(ward_lga_means,
          by = "ward_lga")

prediction_set_nolon = merge(prediction_set_nolon,
                             ward_lga_means,
                             by = "ward_lga",
                             all.x = TRUE)

summary(prediction_set_nolon$latitude_ward_lga_mean)
summary(prediction_set_nolon$longitude_ward_lga_mean)

prediction_set_nolon$latitude_best_mean = coalesce(prediction_set_nolon$latitude_subvillage_ward_mean,
                                                   prediction_set_nolon$latitude_ward_lga_mean)
prediction_set_nolon$longitude_best_mean = coalesce(prediction_set_nolon$longitude_subvillage_ward_mean,
                                                   prediction_set_nolon$longitude_ward_lga_mean)

summary(prediction_set_nolon$latitude_best_mean)
summary(prediction_set_nolon$longitude_best_mean)


#Create lga averages for lat lon

lga_means = prediction_set_lon %>%
    group_by(lga) %>%
    summarise(longitude_lga_mean = mean(longitude))

lga_means = prediction_set_lon %>%
    group_by(lga) %>%
    summarise(latitude_lga_mean = mean(latitude)) %>%
    merge(lga_means,
          by = "lga")

prediction_set_nolon = merge(prediction_set_nolon,
                             lga_means,
                             by = "lga",
                             all.x = TRUE)

summary(prediction_set_nolon$latitude_lga_mean)
summary(prediction_set_nolon$longitude_lga_mean)

prediction_set_nolon$latitude_best_mean = coalesce(prediction_set_nolon$latitude_best_mean,
                                                   prediction_set_nolon$latitude_lga_mean)
prediction_set_nolon$longitude_best_mean = coalesce(prediction_set_nolon$longitude_best_mean,
                                                   prediction_set_nolon$longitude_lga_mean)

summary(prediction_set_nolon$latitude_best_mean)
summary(prediction_set_nolon$longitude_best_mean)



#Create region averages for lat lon

region_means = prediction_set_lon %>%
    group_by(region) %>%
    summarise(longitude_region_mean = mean(longitude))

region_means = prediction_set_lon %>%
    group_by(region) %>%
    summarise(latitude_region_mean = mean(latitude)) %>%
    merge(region_means,
          by = "region")

prediction_set_nolon = merge(prediction_set_nolon,
                             region_means,
                             by = "region",
                             all.x = TRUE)

summary(prediction_set_nolon$latitude_region_mean)
summary(prediction_set_nolon$longitude_region_mean)

prediction_set_nolon$latitude_best_mean = coalesce(prediction_set_nolon$latitude_best_mean,
                                                   prediction_set_nolon$latitude_region_mean)
prediction_set_nolon$longitude_best_mean = coalesce(prediction_set_nolon$longitude_best_mean,
                                                    prediction_set_nolon$longitude_region_mean)

summary(prediction_set_nolon$latitude_best_mean)
summary(prediction_set_nolon$longitude_best_mean)

summary(prediction_set_lon$latitude)
summary(prediction_set_lon$longitude)


#Join new lat/lon back

prediction_set_nolon$latitude = prediction_set_nolon$latitude_best_mean
prediction_set_nolon$longitude = prediction_set_nolon$longitude_best_mean

prediction_set_working = prediction_set_nolon %>%
    select(-contains("mean")) %>%
    rbind(prediction_set_lon)

rm(prediction_set_lon, 
   prediction_set_nolon,
   subvillage_means,
   ward_means, 
   region_means,
   lga_means)

chart_data = ggplot(prediction_set_working)

chart_data +
    geom_density(aes(x = longitude))

chart_data +
    geom_density(aes(x = latitude))

tanzania_map = get_map(location = "tanzania", maptype = "roadmap",zoom = 6)

ggmap(tanzania_map, extent = "device") + geom_point(aes(x = longitude, y = latitude), colour = "red", 
                                                 alpha = 0.05, size = 0.2, data = prediction_set_working)
ggmap(tanzania_map, extent = "device") + 
    geom_density2d(data = prediction_set_working, 
                   aes(x = longitude, y = latitude), 
                   size = 0.3) +
    stat_density2d(data = prediction_set_working, 
                   aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), 
                   size = 0.2, 
                   bins = 40, 
                   geom = "polygon") +
    scale_fill_gradient(low = "green", 
                        high = "red") + 
    scale_alpha(range = c(0, 0.7), 
                guide = FALSE)



#Altitude

chart_data = ggplot(prediction_set_working)

chart_data +
    geom_density(aes(x = gps_height))

sum(prediction_set_working$gps_height == 0)


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

prediction_set_working = prediction_set_working %>%
    rename(gps_height_metres = gps_height)

altitude_knn_set = prediction_set[!prediction_set_working$latitude == 0 & !prediction_set_working$gps_height == 0,c("id","gps_height","longitude","latitude")]

altitude_knn_set_test = sample_n(altitude_knn_set,nrow(altitude_knn_set)/10)
altitude_knn_set_train = altitude_knn_set[!altitude_knn_set$id %in% altitude_knn_set_test$id,]

set.seed(80085)
rm(knn_performance)

    for(i in 1:7) {
altitude_knn_model = knn.reg(
    train = altitude_knn_set_train[,c("longitude","latitude")], 
    test = altitude_knn_set_test[,c("longitude","latitude")],
    y =altitude_knn_set_train$gps_height,
    k = i
    )
MSE = mean((altitude_knn_set_test$gps_height - altitude_knn_model$pred)^2)
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

prediction_set_working_nonalti = prediction_set_working[prediction_set_working$gps_height_metres == 0,]
prediction_set_working_alti = prediction_set_working[!prediction_set_working$gps_height_metres == 0,]

altitude_best_knn_model = knn.reg(
    train = prediction_set_working_alti[,c("longitude","latitude")], 
    test = prediction_set_working_nonalti[,c("longitude","latitude")],
    y =prediction_set_working_alti$gps_height_metres,
    k = best_K
)

table(altitude_best_knn_model$pred)

prediction_set_working_nonalti$gps_height_metres = altitude_best_knn_model$pred

set.seed(80085)
new_height_sample = sample_n(
    prediction_set_working_nonalti[,c("gps_height_metres","latitude","longitude")],5)

new_height_sample = new_height_sample %>%
    cbind(online_height = c(1297,
                            502,
                            1430,
                            1354,
                            1309)
    )
new_height_sample$height_diff = new_height_sample$gps_height - new_height_sample$online_height
prediction_set_working = rbind(prediction_set_working_nonalti, prediction_set_working_alti)

#-------installer

length(levels(prediction_set_working$installer))

c(as.character(unique(prediction_set_working[grep("Losa",prediction_set_working$installer),"installer"])))

installer_n_obs = prediction_set_working$installer %>%
    table() %>%
    data.frame() %>%
    arrange(desc(Freq))

installer_n_obs %>%
    filter(Freq > 500) 

prediction_set_working$installer_water_engineer = 0
prediction_set_working[prediction_set_working$installer %in% c("DWE","RWE"),"installer_water_engineer"] = 1

#-------funder

length(levels(prediction_set_working$funder))

c(as.character(unique(prediction_set_working[grep("Losa",prediction_set_working$funder),"funder"])))

funder_n_obs = prediction_set_working$funder %>%
    table() %>%
    data.frame() %>%
    arrange(desc(Freq))

funder_n_obs %>%
    filter(Freq > 500) 

prediction_set_working$installer[prediction_set_working$installer == ''] = 'unknown_installer'
prediction_set_working$installer[prediction_set_working$funder == ''] = 'unknown_funder'
prediction_set_working$installer = tolower(prediction_set_working$installer)
prediction_set_working$funder = tolower(prediction_set_working$funder)
prediction_set_working$installer = gsub("[^[:alnum:] ]","",prediction_set_working$installer)
prediction_set_working$funder = gsub("[^[:alnum:] ]","",prediction_set_working$funder)
prediction_set_working$installer = gsub("  "," ",prediction_set_working$installer)
prediction_set_working$funder = gsub("  "," ",prediction_set_working$funder)
prediction_set_working$funder = trimws(prediction_set_working$funder)
prediction_set_working$installer = trimws(prediction_set_working$installer)
prediction_set_working$installer[prediction_set_working$installer == ''] = 'unknown_installer'
prediction_set_working$installer[prediction_set_working$funder == ''] = 'unknown_funder'



funder_installer_similarity = stringdist(prediction_set_working$funder, 
                                         prediction_set_working$installer,
                                         method = "osa")

prediction_set_working$funder_installer = 0

prediction_set_working$funder_installer[funder_installer_similarity <= 2] = 1

prediction_set_working[prediction_set_working$funder_installer == 1,c("funder_installer","funder","installer")]

sum(
    as.character(prediction_set_working$funder) == as.character(prediction_set_working$installer) & 
        !prediction_set_working$funder %in% c('',' '))
sum(prediction_set_working$funder == prediction_set_working$installer)
sum(prediction_set_working$funder_installer == 1)

#-------construction year

table(prediction_set_working$construction_year)

prediction_set_working$construction_year = as.numeric(prediction_set_working$construction_year)

chart_data = ggplot(prediction_set_working[prediction_set_working$construction_year != 0,])

chart_data+
    geom_bar(aes(x = construction_year))

chart_data+
    geom_bar(aes(x = construction_year, y= id, fill = status_group),position = "fill", stat = "identity") + 
    scale_y_continuous()

prediction_set_working$construction_decade = 'None'
prediction_set_working$construction_decade[prediction_set_working$construction_year != 0] = 
    paste0(substr(prediction_set_working$construction_year[prediction_set_working$construction_year != 0],1,3),"0s")

chart_data = ggplot(prediction_set_working)

chart_data+
    geom_bar(aes(x = construction_decade, y= id, fill = status_group),position = "fill", stat = "identity") + 
    scale_y_continuous()

#--------Total Static Head

x = prediction_set_working[prediction_set_working$amount_tsh != 0,]
aggregate(amount_tsh ~ waterpoint_type, x, median)

chart_data = ggplot(x)

chart_data +
    geom_density(aes(x = amount_tsh)) +
    facet_grid(waterpoint_type ~ .)
max(prediction_set_working$amount_tsh)


#--------Basin

table(prediction_set_working$basin)

#--------Source

table(prediction_set_working$source)
table(prediction_set_working$source_type)
table(prediction_set_working$source_class)

chart_data = ggplot(
    prediction_set_working[
        prediction_set_working$source %in% c("lake","river","machine dbh","hand dtw","dam","other"),])


prediction_set_working$source[
    !prediction_set_working$source %in% c("lake","river","machine dbh","hand dtw","dam","other")] = 'other'

#----------Quantity

table(prediction_set_working$quantity)
table(prediction_set_working$quantity_group)

#----------Quality

table(prediction_set_working$quality_group)
table(prediction_set_working$water_quality)

prediction_set_working$abandoned_quality = 'other'
prediction_set_working$abandoned_quality[grep("abandoned", prediction_set_working)] = 'abandoned'
prediction_set_working = prediction_set_working %>%
    select(-water_quality)

#----------Extraction Method


table(prediction_set_working$extraction_type)
table(prediction_set_working$extraction_type_group)
table(prediction_set_working$extraction_type_class)


chart_data = ggplot(prediction_set_working)

chart_data+
    geom_bar(aes(x = extraction_type_group, y= id, fill = status_group),position = "fill", stat = "identity") + 
    scale_y_continuous() +
    facet_grid(~extraction_type_class, strip.position = "bottom", scales = "free_x")

#-------------Water point type

table(prediction_set_working$waterpoint_type)
table(prediction_set_working$waterpoint_type_group)




#Scheme MAnagement
summary(prediction_set_working$scheme_management)

chart_data = ggplot(prediction_set_working)

chart_data + geom_bar(aes(x = scheme_management, fill = status_group))

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

prediction_set_working[,c("scheme_management","scheme_management_broad")]
prediction_set_working$scheme_management_broad = prediction_set_working$scheme_management_broad %>%
    as.factor()

summary(prediction_set_working$scheme_management_broad)
summary(prediction_set_working$scheme_management)

prediction_set_working$scheme_management[is.na(prediction_set_working$scheme_management_broad)]


chart_data = ggplot(prediction_set_working)

chart_data +
    geom_bar(aes(x = scheme_management_broad, fill = status_group))

#-----------Scheme Name

length(levels(prediction_set_working$scheme_name))

prediction_set_working$scheme_name %>%
    table() %>%
    data.frame() %>%
    arrange(desc(Freq))

#-----------Management


table(prediction_set_working$management)
table(prediction_set_working$management_group)

#-----------Payment

table(prediction_set_working$payment)
table(prediction_set_working$payment_type)

#-----------Permit

table(prediction_set_working$permit)
prediction_set_working$permit_clean[prediction_set_working$permit == ''] = 'unknown'
prediction_set_working$permit_clean = prediction_set_working$permit
prediction_set_working$permit_clean[prediction_set_working$permit == ''] = 'unknown'
prediction_set_working$permit = prediction_set_working$permit_clean
prediction_set_working = prediction_set_working %>%
    select(-permit_clean)

#-----------Public Meeting

table(prediction_set_working$public_meeting)
prediction_set_working$public_meeting_clean[prediction_set_working$public_meeting == ''] = 'unknown'
prediction_set_working$public_meeting_clean = prediction_set_working$public_meeting
prediction_set_working$public_meeting_clean[prediction_set_working$public_meeting == ''] = 'unknown'
prediction_set_working$public_meeting = prediction_set_working$public_meeting_clean
prediction_set_working = prediction_set_working %>%
    select(-public_meeting_clean)

#------------Population


summary(prediction_set_working$population)

chart_data = ggplot(prediction_set_working[prediction_set_working$population != 0 & prediction_set_working$population < 1000,])

chart_data + 
    geom_density(aes(x = population)) +
    facet_grid(status_group ~ .)

sum(prediction_set_working$population == 0)

prediction_set_working$population[prediction_set_working$population == 0] = NA
prediction_set_working$population_cat = 
    bin(prediction_set_working$population[prediction_set_working$population != 0],
    5,
    method="length",
    na.omit = FALSE)

table(prediction_set_working$population_cat)



population_knn_set = prediction_set[!prediction_set_working$latitude == 0 & !prediction_set_working$population == 0,c("id","population","longitude","latitude")]

population_knn_set_test = sample_n(population_knn_set,nrow(population_knn_set)/10)
population_knn_set_train = population_knn_set[!population_knn_set$id %in% population_knn_set_test$id,]

set.seed(80085)
rm(knn_performance)

for(i in 1:7) {
    population_knn_model = knn.reg(
        train = population_knn_set_train[,c("longitude","latitude")], 
        test = population_knn_set_test[,c("longitude","latitude")],
        y =population_knn_set_train$population,
        k = i
    )
    MSE = mean((population_knn_set_test$population - population_knn_model$pred)^2)
    K = population_knn_model$k
    if(!exists("knn_performance")) {
        knn_performance = 
            data.frame(K, MSE)
    } else {
        knn_performance = knn_performance %>%
            rbind(c(K, MSE))
    }
}

best_K = knn_performance$K[knn_performance$MSE == min(knn_performance$MSE)]

prediction_set_working_nonpopulation = prediction_set_working[prediction_set_working$population == 0,]
prediction_set_working_population = prediction_set_working[!prediction_set_working$population == 0,]

population_best_knn_model = knn.reg(
    train = prediction_set_working_population[,c("longitude","latitude")], 
    test = prediction_set_working_nonpopulation[,c("longitude","latitude")],
    y =prediction_set_working_population$population,
    k = best_K
)

table(population_best_knn_model$pred)

prediction_set_working_nonpopulation$population = population_best_knn_model$pred
