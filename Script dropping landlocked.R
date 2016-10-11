# Kuni Mariculture - dropping landlocked countries from data set
# Molly Wilson 10.10.16

data_normalized_coastal <- data_normalized_101016 [ ! data_normalized_101016$country %in% c("Afghanistan", "Andorra","Armenia","Austria","Azerbaijan","Belarus","Bhutan","Bolivia","Botswana","Burkina Faso", "Burundi", "Central African Republic","Chad","Czech Republic","Czechoslovakia","Ethiopia","Ethiopia PDR","Hungary","Kazakhstan","Kosovo","Kyrgyzstan","Laos","Lesotho","Liechtenstein","Luxembourg","Macedonia","Malawi","Mali","Moldova","Mongolia","Nepal","Niger","Paraguay","Rwanda","San Marino","Serbia","Serbia and Montenegro","Slovakia","South Ossetia","South Sudan","Swaziland","Switzerland","Tajikistan","Turkmenistan","Uganda","Uzbekistan","Vatican City","Zambia","Zimbabwe"),]

write.csv(data_normalized_coastal, file= "data_normalized_coastal.csv")