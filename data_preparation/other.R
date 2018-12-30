

summary(prediction_set$gps_height_metres)
log10(prediction_set[!prediction_set$amount_tsh == 0,"amount_tsh"])


dupe_locs = aggregate(id ~ longitude + latitude, prediction_set, length) %>%
    .[.$id > 1,] %>%
    .[!.$longitude == 0,]

dupe_locs_data = merge(dupe_locs,
                       prediction_set,
                       by = c("longitude","latitude"),
                       all.x = TRUE,
                       all.y = FALSE)

#Recorded By
summary(prediction_set_working$recorded_by)

levels(prediction_set_working$scheme_management)

colnames(prediction_set_working)

#Extraction Type

chart_data = ggplot(prediction_set_working)

chart_data +
    geom_bar(aes(x = extraction_type_class, fill = status_group))

levels(prediction_set_working$extraction_type)
levels(prediction_set_working$extraction_type_group)
levels(prediction_set_working$extraction_type_class)

table(prediction_set_working$extraction_type_class)

#Construction Year

table(prediction_set_working$construction_year)

#payment

#Payment is duplicated

table(prediction_set_working$payment)
table(prediction_set_working$payment_type)

#Payment is duplicated
table(prediction_set_working$payment)
table(prediction_set_working$payment_type)

#Quality

table(prediction_set_working$water_quality)
table(prediction_set_working$quality_group)

chart_data = aggregate(
    id ~ water_quality + status_group, 
    prediction_set_working, 
    length) %>%
    rename(observations = id) %>%
    ggplot()

chart_data + 
    geom_bar(aes(
        x=water_quality, 
        fill = status_group, 
        y  = observations), 
        position = "fill",
        stat = "identity")

#Quantity
#Quantity is duplicated
table(prediction_set_working$quantity)
table(prediction_set_working$quantity_group)

#Quantity
#Quantity is duplicated
table(prediction_set_working$quality_group)
table(prediction_set_working$quality)


#Source
table(prediction_set_working$source)
table(prediction_set_working$source_type)
table(prediction_set_working$source_class)

table(prediction_set_working[,c("source","source_type")])
table(prediction_set_working[,c("source_class","source_type")])


chart_data = aggregate(
    id ~ source_type + status_group + source_class, 
    prediction_set_working, 
    length) %>%
    rename(observations = id) %>%
    ggplot()

chart_data + 
    geom_bar(aes(
        x = source_type, 
        fill = status_group, 
        y  = observations), 
        position = "fill",
        stat = "identity") +
    facet_grid(source_class ~ .)

#Population

summary(prediction_set_working$population)

chart_data = ggplot(prediction_set_working)

sum(prediction_set_working$population == 0)



#Funder

aggregate(id ~ funder, prediction_set, length)


#Scheme Name


a_few_bag_eggs = c(
    grep('Kisale Kitale',prediction_set$scheme_name),
    grep('New keni',prediction_set$scheme_name))

unique(prediction_set[a_few_bag_eggs,"scheme_name"])

tolower()
prediction_set$scheme_name_clean = prediction_set$scheme_name
prediction_set$scheme_name_clean = tolower(prediction_set$scheme_name_clean)
prediction_set$scheme_name_clean = gsub("water","",prediction_set$scheme_name_clean)
prediction_set$scheme_name_clean = gsub("wate","",prediction_set$scheme_name_clean)

prediction_set$scheme_name_clean = gsub("supply","",prediction_set$scheme_name_clean)
prediction_set$scheme_name_clean = gsub("suppl","",prediction_set$scheme_name_clean)

prediction_set$scheme_name_clean = gsub("scheme","",prediction_set$scheme_name_clean)
prediction_set$scheme_name_clean = gsub("schem","",prediction_set$scheme_name_clean)
prediction_set$scheme_name_clean = gsub("gravity","",prediction_set$scheme_name_clean)
prediction_set$scheme_name_clean = gsub("borehole","",prediction_set$scheme_name_clean)
prediction_set$scheme_name_clean = gsub("pipeline","",prediction_set$scheme_name_clean)
prediction_set$scheme_name_clean = gsub("source","",prediction_set$scheme_name_clean)
prediction_set$scheme_name_clean = gsub("project","",prediction_set$scheme_name_clean)
prediction_set$scheme_name_clean = gsub("spring","",prediction_set$scheme_name_clean)
prediction_set$scheme_name_clean = gsub("basin","",prediction_set$scheme_name_clean)
prediction_set$scheme_name_clean = gsub("group","",prediction_set$scheme_name_clean)

prediction_set$scheme_name_clean = gsub(" ","",prediction_set$scheme_name_clean)

table(prediction_set$scheme_name_clean[!prediction_set$scheme_name_clean == ""])

table(prediction_set$scheme_name,prediction_set$scheme_management) %>%
    as.data.frame() %>%
    filter(Freq > 0) %>%
    .$Var1 %>%
    table() %>%
    as.data.frame() %>%
    filter(Freq > 1) %>%
    nrow()


#Total Static Head
a = aggregate(id ~ amount_tsh, prediction_set, length) %>%
    as.table()

chart_data = ggplot(prediction_set)

chart_data + 
    geom_histogram(aes(x=amount_tsh),
                   binwidth = 1000)

chart_data = ggplot(prediction_set[!prediction_set$amount_tsh == 0 & prediction_set$amount_tsh < 5000,])

chart_data + 
    geom_histogram(aes(x=amount_tsh),
                   bins = 20)

chart_data = ggplot(prediction_set)

chart_data + 
    geom_histogram(aes(x=log10(amount_tsh), fill=status_group),
                   bins = 10)

prediction_set$amount_tshlog10 = floor(log10(prediction_set$amount_tsh)*10/5)*5/10

prediction_set[is.infinite(prediction_set$amount_tshlog10),"amount_tshlog10"] = 0

test = aggregate(id ~ status_group + amount_tshlog10, prediction_set,length)


ggplot(test,aes(x = amount_tshlog10, y= id, fill = status_group)) + 
    geom_bar(position = "fill", stat = "identity") + 
    scale_y_continuous()

prediction_set$amount_tsh_20 = floor(prediction_set$amount_tsh/50)*50

test = aggregate(id ~ status_group + amount_tsh_20, prediction_set,length) %>%
    .[.$amount_tsh > 0 & .$amount_tsh <= 300,]

ggplot(test,aes(x = amount_tsh_20, y= id, fill = status_group)) + 
    geom_bar(position = "fill", stat = "identity") + 
    scale_y_continuous()

17402/59400
