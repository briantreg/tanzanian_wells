
chart_statusfill_obs = function(data, x, colname) {
    chart_count <<- chart_count + 1
    title_name = paste0('Diagram ',chart_count,' - Observed ',colname,' By Well Status')
    y_name = paste0('Observations')
    
    ggplot(data) + 
    geom_bar(aes_string(x = x,fill = "status_group")) + 
    scale_fill_manual(values = c("firebrick","orangered","forestgreen")) +
    theme_bw() +
    labs(x = colname, fill = 'Well Status',
         title = title_name, y = y_name)+
    theme(panel.border = element_rect(fill = NA, colour = NA), 
          axis.line = element_line(colour = 'grey13'), 
          panel.grid.major = element_line(colour = NA), 
          panel.grid.minor = element_line(colour = NA))
}

chart_obs = function(data, x,colname) {
    chart_count <<- chart_count + 1
    title_name = paste0('Diagram ',chart_count,' - Observations by ',colname)
    y_name = paste0('Observations')
    ggplot(data) + 
        geom_bar(aes_string(x = x))+
    theme_bw() +
    labs(x = colname, title = title_name, y = y_name)+
    theme(panel.border = element_rect(fill = NA, colour = NA), 
          axis.line = element_line(colour = 'grey13'), 
          panel.grid.major = element_line(colour = NA), 
          panel.grid.minor = element_line(colour = NA))
}

chart_obs_agg = function(data, x,colname) {
    chart_count <<- chart_count + 1
    title_name = paste0('Diagram ',chart_count,' - Number Of ',colname,'s By Observations Recorded')
    y_name = paste0(colname,'s')
    x_name = paste0('Observations Recorded')
    
    ggplot(data) + 
        geom_bar(aes_string(x = x))+
        theme_bw() +
        labs(x = x_name, title = title_name, y = y_name)+
        theme(panel.border = element_rect(fill = NA, colour = NA), 
              axis.line = element_line(colour = 'grey13'), 
              panel.grid.major = element_line(colour = NA), 
              panel.grid.minor = element_line(colour = NA))
}

chart_density = function(data, x, colname) {
    chart_count <<- chart_count + 1
    title_name = paste0('Diagram ',chart_count,' - Observations by ',colname)
    y_name = paste0('Observation Density')
    
    ggplot(data) + 
        geom_density(aes_string(x = x), fill = 'grey13') +
         theme_bw() +
         labs(x = colname, title = title_name, y = y_name)+
         theme(panel.border = element_rect(fill = NA, colour = NA), 
               axis.line = element_line(colour = 'grey13'), 
               panel.grid.major.x = element_line(colour = NA), 
               panel.grid.minor = element_line(colour = NA))
}

chart_status_obs_facetrow = function(data, x, facet, facetScale = "fixed", colname) {
    chart_statusfill_obs(data, x, colname) + 
    facet_grid(facet, scales = facetScale)
}



table_char_freq = function(data, col, names) {
    upnames = c(names, "Observations")
        data %>%
        select_(col) %>%
        table() %>%
        as.data.frame() %>%
        arrange(desc(Freq)) %>%
        setNames(upnames) 
}


drop_col = function(data, col) {
    data %>%
    select_(.dots = paste('-',col))
}

factor_summary_table = function(table) {
    c(nrow(table),
      round(mean(table[["Observations"]]),0),
      min(table[["Observations"]]),
      max(table[["Observations"]])) %>%
        t() %>%
        as.data.frame() %>%
        setNames(c("Factors",
                   "Mean Observations",
                   "Min Observations",
                   "Max Observations"))
    }

data_buckets = function(data, col, bucket){
    ceiling(data[[col]]/bucket)*bucket
}



mean_latlon = function(data, geo) {
    means = data %>%
    group_by_(geo) %>%
    summarise(longitude_new = mean(longitude))

    means = data %>%
    group_by_(geo) %>%
    summarise(latitude_new = mean(latitude)) %>%
    merge(means,
          by = geo)
    
    means
}

new_latlon = function() {
    prediction_set_nolon$latitude_best <<- coalesce(prediction_set_nolon$latitude_best,
                                                  prediction_set_nolon$latitude_new)
    prediction_set_nolon$longitude_best <<- coalesce(prediction_set_nolon$longitude_best,
                                                   prediction_set_nolon$longitude_new)
    }

find_new_latlon = function(data, mean_data, col){
    data %>%
        merge(mean_latlon(mean_data,col),
              by = col,
              all.x = TRUE)
}

char_clean = function(data, col) {
    x = tolower(data[[col]]) %>%
        gsub("[^[:alpha:] ]","",.) %>%
        gsub('\\b\\w{1}\\b','',.) %>%
        gsub("  "," ",.) %>%
        trimws(.)
    x
}

chart_percent = function(data, col, colname) {
    chart_count <<- chart_count + 1
    title_name = paste0('Diagram ',chart_count,' - Observed ',colname,' By Well Status (%)')
    y_name = paste0( 'Percentage Of ',colname,' Group')
    
    form = paste0('id ~ ',col,' + status_group') %>%
        as.formula()
    
    chart_data = aggregate(
        form, 
        data, 
        length) %>%
    rename(observations = id) %>%
    ggplot()
    
    chart_data + geom_bar(
        aes_string(x = col,fill = "status_group", y  = "observations"),
    position = "fill",
    stat = "identity") +
    scale_fill_manual(values = c("firebrick","orangered","forestgreen")) +
    theme_bw() +
    labs(x = colname, fill = 'Well Status',
         title = title_name, y = y_name)+
    theme(panel.border = element_rect(fill = NA, colour = NA), 
          axis.line = element_line(colour = 'grey13'), 
          panel.grid.major = element_line(colour = NA), 
          panel.grid.minor = element_line(colour = NA))
}
