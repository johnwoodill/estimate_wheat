library(lfe)
library(systemfit)

cropdat <- readRDS("data/reg_data.rds")

mod <- felm(ln_rev_wheat ~ dday0_10*wheat_irrigated_a + dday10_30*wheat_irrigated_a + dday30*wheat_irrigated_a + prec + prec_sq + state:trend| fips | 0 | state, data = rdat, weights = rdat$w)
summary(mod)


dmdat <- select(cropdat, ln_rev_corn, ln_rev_cotton, ln_rev_hay, ln_rev_soybean, ln_rev_wheat, 
                dday0_10, dday10_30, dday30, prec, prec_sq, trend, trend_sq,
                dday0_10_rm , dday10_30_rm , dday30_rm ,  prec_rm , prec_sq_rm ,
                dday0_10w, dday10_30w, dday30w,
                trend_lat, trend_long, trend_sq_lat, trend_sq_long,
                trend1_al , trend1_ar , trend1_de , trend1_ga , trend1_ia , trend1_il ,
  trend1_in , trend1_ks , trend1_ky , trend1_md , trend1_mi , trend1_mn ,
  trend1_mo , trend1_ms , trend1_nc , trend1_nd , trend1_ne , trend1_oh ,
  trend1_ok , trend1_sc , trend1_sd , trend1_tn , trend1_va , trend1_wi ,
  trend1_wv)

# Weight data from acres
w <- sqrt(1 + cropdat$w)
dmdat <- dmdat*w

cropdat_dm <- demeanlist(dmdat, fl = list(fips = factor(cropdat$fips)))


mod1 <- ln_rev_corn ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
  trend1_al + trend1_ar + trend1_de + trend1_ga + trend1_ia + trend1_il +
  trend1_in + trend1_ks + trend1_ky + trend1_md + trend1_mi + trend1_mn +
  trend1_mo + trend1_ms + trend1_nc + trend1_nd + trend1_ne + trend1_oh +
  trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_va + trend1_wi +
  trend1_wv - 1


mod2 <- ln_rev_cotton ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
trend1_al + trend1_ar + trend1_de + trend1_ga + trend1_ia + trend1_il +
  trend1_in + trend1_ks + trend1_ky + trend1_md + trend1_mi + trend1_mn +
  trend1_mo + trend1_ms + trend1_nc + trend1_nd + trend1_ne + trend1_oh +
  trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_va + trend1_wi +
  trend1_wv - 1
 

mod3 <- ln_rev_hay ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
trend1_al + trend1_ar + trend1_de + trend1_ga + trend1_ia + trend1_il +
  trend1_in + trend1_ks + trend1_ky + trend1_md + trend1_mi + trend1_mn +
  trend1_mo + trend1_ms + trend1_nc + trend1_nd + trend1_ne + trend1_oh +
  trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_va + trend1_wi +
  trend1_wv - 1


mod4 <- ln_rev_soybean ~ dday0_10 + dday10_30 + dday30 +  prec + prec_sq +
trend1_al + trend1_ar + trend1_de + trend1_ga + trend1_ia + trend1_il +
  trend1_in + trend1_ks + trend1_ky + trend1_md + trend1_mi + trend1_mn +
  trend1_mo + trend1_ms + trend1_nc + trend1_nd + trend1_ne + trend1_oh +
  trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_va + trend1_wi +
  trend1_wv - 1


mod5 <- ln_rev_wheat ~ dday0_10 + dday10_30 + dday30 + prec + prec_sq +
trend1_al + trend1_ar + trend1_de + trend1_ga + trend1_ia + trend1_il +
  trend1_in + trend1_ks + trend1_ky + trend1_md + trend1_mi + trend1_mn +
  trend1_mo + trend1_ms + trend1_nc + trend1_nd + trend1_ne + trend1_oh +
  trend1_ok + trend1_sc + trend1_sd + trend1_tn + trend1_va + trend1_wi +
  trend1_wv - 1

mod <- systemfit(list(corn = mod1, 
                       cotton = mod2, 
                       hay = mod3, 
                       soybean = mod4,
                       wheat = mod5), data = cropdat_dm, method = "SUR")

summary(mod)
