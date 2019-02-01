


cropdat <- readRDS("data/reg_data.rds")


dmdat <- select(cropdat, z_corn_a, z_cotton_a, z_hay_a, z_soybean_a, z_wheat_a, 
                dday0_10, dday10_30, dday30, prec, prec_sq, trend, trend_sq,
                dday0_10_rm , dday10_30_rm , dday30_rm , prec_rm , prec_sq_rm, 
                dday0_10_rmw , dday10_30_rmw , dday30_rmw , prec_rmw , prec_sq_rmw,
                trend_lat, trend_long, trend_sq_lat, trend_sq_long, trend1_al , 
                trend1_ar , trend1_de , trend1_ga , trend1_ia , trend1_il ,
  trend1_in , trend1_ks , trend1_ky , trend1_md , trend1_mi , trend1_mn ,
  trend1_mo , trend1_ms , trend1_nc , trend1_nd , trend1_ne , trend1_oh ,
  trend1_ok , trend1_sc , trend1_sd , trend1_tn , trend1_va , trend1_wi ,
  trend1_wv)

cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))

cropdat_means <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)), means = TRUE)

# Weight data from acres
w <- sqrt(cropdat$w)
dmdat <- dmdat*w


mod1 <- z_corn_a ~ 
  dday0_10_rm + dday10_30_rm + dday30_rm + prec_rm + prec_sq_rm +
    trend1_al + trend1_ar + trend1_de + trend1_ga + trend1_ia + trend1_il +
  trend1_in + trend1_ks + trend1_ky + trend1_md + trend1_mi + trend1_mn +
  trend1_mo + trend1_ms + trend1_nc + trend1_nd + trend1_ne + trend1_oh +
  trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_va + trend1_wi +
  trend1_wv - 1


mod2 <- z_cotton_a ~   
  dday0_10_rm + dday10_30_rm + dday30_rm + prec_rm + prec_sq_rm +
    trend1_al + trend1_ar + trend1_de + trend1_ga + trend1_ia + trend1_il +
  trend1_in + trend1_ks + trend1_ky + trend1_md + trend1_mi + trend1_mn +
  trend1_mo + trend1_ms + trend1_nc + trend1_nd + trend1_ne + trend1_oh +
  trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_va + trend1_wi +
  trend1_wv  - 1


mod3 <- z_hay_a ~  
  dday0_10_rm + dday10_30_rm + dday30_rm + prec_rm + prec_sq_rm +
    trend1_al + trend1_ar + trend1_de + trend1_ga + trend1_ia + trend1_il +
  trend1_in + trend1_ks + trend1_ky + trend1_md + trend1_mi + trend1_mn +
  trend1_mo + trend1_ms + trend1_nc + trend1_nd + trend1_ne + trend1_oh +
  trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_va + trend1_wi +
  trend1_wv  - 1


mod4 <- z_soybean_a ~  
  dday0_10_rm + dday10_30_rm + dday30_rm + prec_rm + prec_sq_rm +
    trend1_al + trend1_ar + trend1_de + trend1_ga + trend1_ia + trend1_il +
  trend1_in + trend1_ks + trend1_ky + trend1_md + trend1_mi + trend1_mn +
  trend1_mo + trend1_ms + trend1_nc + trend1_nd + trend1_ne + trend1_oh +
  trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_va + trend1_wi +
  trend1_wv  - 1



mod5 <- z_wheat_a ~  
  dday0_10_rm + dday10_30_rm + dday30_rm + prec_rm + prec_sq_rm +
    trend1_al + trend1_ar + trend1_de + trend1_ga + trend1_ia + trend1_il +
  trend1_in + trend1_ks + trend1_ky + trend1_md + trend1_mi + trend1_mn +
  trend1_mo + trend1_ms + trend1_nc + trend1_nd + trend1_ne + trend1_oh +
  trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_va + trend1_wi +
  trend1_wv - 1

ten_mod <- systemfit(list(corn = mod1, 
                          cotton = mod2, 
                          hay = mod3, 
                          soybean = mod4,
                          wheat = mod5), data = cropdat_dm, method = "SUR")

summary(ten_mod)
