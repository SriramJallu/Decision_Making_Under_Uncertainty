rm(list=ls())
library(terra)
library(sf)
library(caret)
library(randomForest)
library(car)
library(gstat)
library(sp)

set.seed(12345)


residuals_df <- "Load your residuals file"

# Check for Correlation
correaltion_matrix <- cor(residuals_df, use = "pairwise.complete.obs")

print(round(correaltion_matrix, 4))
class(residuals_df)

# Example for pH, SOC, Olsen P and Exch K are shown here.

################################################################################
################################################################################
# VGM for pH
par(mfrow = c(1, 1))

#plot(v_pH_cloud <- variogram(Residuals_pH ~ 1, data = residuals_df, cloud = T))

plot(vg_pH <- variogram(Residuals_pH ~ 1, data = residuals_df), pl = T)


vgm_model_pH <- vgm(nugget = 0.02, psill = 0.2, range = 18000, model = "Sph")
plot(vg_pH, pl = T, model = vgm_model_pH)

vgm_model_fit_pH <- fit.variogram(vg_pH, vgm_model_pH)

vgm_model_fit_pH

plot(vg_pH, pl = T, model = vgm_model_fit_pH, main = "Variogram for pH")

################################################################################
################################################################################

## VGM Model for SOC
#plot(v_soc_cloud <- variogram(Residuals_Carbon ~ 1, data = residuals_df, cloud = T))
plot(vg_soc <- variogram(Residuals_Carbon ~ 1, data = residuals_df), pl = T)

vgm_model_soc <- vgm(nugget = 23, psill = 50, range = 10000, model = "Sph")

plot(vg_soc, vgm_model_soc)

vgm_model_fit_soc <- fit.variogram(vg_soc, vgm_model_soc)
vgm_model_fit_soc
plot(vg_soc, pl = T, model = vgm_model_fit_soc)


################################################################################
################################################################################
## VGM Model for OlsenP

plot(vg_olsenp <- variogram(Residuals_OlsenP ~ 1, data = residuals_df), pl = T)

vgm_model_olsenp <- vgm(nugget = 2, psill = 8, range = 20000, model = "Sph")
plot(vg_olsenp, vgm_model_olsenp)

vgm_model_fit_olsenp <- fit.variogram(vg_olsenp, vgm_model_olsenp)
vgm_model_fit_olsenp
plot(vg_olsenp, pl = T, model = vgm_model_fit_olsenp)


################################################################################
################################################################################
## VGM Model for ExchK

vg_exchk <- variogram(Residuals_ExchK ~ 1, data = residuals_df)

vg_exchk$gamma <- vg_exchk$gamma * 10000

plot(vg_exchk, pl = T, main = "Scaled Vario")

vgm_model_exchk <- vgm(nugget = 0.000014 * 10000, psill = 0.00001 * 10000, range = 12000, model = "Sph")
plot(vg_exchk, vgm_model_exchk)

vgm_model_fit_exchk <- fit.variogram(vg_exchk, vgm_model_exchk)
vgm_model_fit_exchk
plot(vg_exchk, vgm_model_fit_exchk)


################################################################################
################################################################################

# gstat object for all variabels
rm(g)
g <- gstat(NULL, id = "pH", form = Residuals_pH ~ 1, data = residuals_df)
g <- gstat(g, id = "SOC", form = Residuals_Carbon ~ 1, data = residuals_df)
g <- gstat(g, id = "OlsenP", form = Residuals_OlsenP ~ 1, data = residuals_df)
g <- gstat(g, id = "ExchK", form = Residuals_ExchK ~ 1, data = residuals_df)
g

v_cross <- variogram(g)
plot(v_cross, pl = T)

rm(g_pH)
g_pH <- gstat(g, id = "pH", model = vgm_model_fit_pH, fill.all = T)
g_pH

g_pH <- fit.lmc(v_cross, g, vgm_model_fit_pH, fit.method = 7, correct.diagonal = 1.01)
g_pH

plot(variogram(g_pH), model = g_pH$model)

pred_raster <- "Read raster grid"
prediction_grid <- as.data.frame(pred_raster, xy = TRUE, cells = TRUE)
head(prediction_grid)
prediction_grid <- prediction_grid[, c("x", "y")]
coordinates(prediction_grid) <- ~ x + y
proj4string(prediction_grid) <- CRS(target_crs)

#length(prediction_grid)

cokrige_all_vars <- predict(g_pH, prediction_grid)


summary(cokrige_all_vars$pH.pred)
summary(cokrige_all_vars$SOC.pred)
summary(cokrige_all_vars$OlsenP.pred)
summary(cokrige_all_vars$ExchK.pred)

hist(cokrige_all_vars$pH.pred)
hist(cokrige_all_vars$SOC.pred)
hist(cokrige_all_vars$OlsenP.pred)
hist(cokrige_all_vars$ExchK.pred)

hist(cokrige_all_vars$pH.var)
hist(cokrige_all_vars$SOC.var)
hist(cokrige_all_vars$OlsenP.var)
hist(cokrige_all_vars$ExchK.var)

prediction_grid$error_preds_pH <- cokrige_all_vars$pH.pred
prediction_grid$error_preds_soc <- cokrige_all_vars$SOC.pred
prediction_grid$error_preds_olsenp <- cokrige_all_vars$OlsenP.pred
prediction_grid$error_preds_exchk <- cokrige_all_vars$ExchK.pred

class(prediction_grid)

#####################################################################################
