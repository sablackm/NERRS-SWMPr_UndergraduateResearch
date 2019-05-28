#Checking Missing Values of Sites for Comparison 

#GND---------------------------------------------------------------------------------------------------------
path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\GulfofMexico"
sitename = 'gndbcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
bc <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\GulfofMexico"
sitename = 'gndblwq'
data_collected <- import_local(path, sitename, trace = FALSE)
bl <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\GulfofMexico"
sitename = 'gndpcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
pc <- qaqc(data_collected)

sub_dat1<- bc[bc$datetimestamp>='2007-01-01 00:00' & bc$datetimestamp<='2017-12-31 23:45',]
g_bc <- ggplot(sub_dat1, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

sub_dat2<- bl[bl$datetimestamp>='2007-01-01 00:00' & bl$datetimestamp<='2017-12-31 23:45',]
g_bl <- ggplot(sub_dat2, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

sub_dat3<- pc[pc$datetimestamp>='2007-01-01 00:00' & pc$datetimestamp<='2017-12-31 23:45',]
g_pc <- ggplot(sub_dat3, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

g_bc
g_bl
g_pc

#Missing DO
missing_bc_do <- round(sum(is.na(sub_dat1$do_mgl))/nrow(sub_dat1)*100,2)
missing_bc_do #MC

missing_bl_do <- round(sum(is.na(sub_dat2$do_mgl))/nrow(sub_dat2)*100,2)
missing_bl_do #OW

missing_pc_do <- round(sum(is.na(sub_dat3$do_mgl))/nrow(sub_dat3)*100,2)
missing_pc_do #OW Not good option, lots of missing data



#SAP------------------------------------------------------------------------------------------------------------------
path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
sitename = 'sapcawq'
data_collected <- import_local(path, sitename, trace = FALSE)
ca <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
sitename = 'sapldwq'
data_collected <- import_local(path, sitename, trace = FALSE)
ld <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
sitename = 'saphdwq'
data_collected <- import_local(path, sitename, trace = FALSE)
hd <- qaqc(data_collected)


sub_dat4<- ca[ca$datetimestamp>='2007-01-01 00:00' & ca$datetimestamp<='2017-12-31 23:45',]
g_ca <- ggplot(sub_dat4, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

sub_dat5<- ld[ld$datetimestamp>='2007-01-01 00:00' & ld$datetimestamp<='2017-12-31 23:45',]
g_ld <- ggplot(sub_dat5, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

sub_dat6<- hd[hd$datetimestamp>='2007-01-01 00:00' & hd$datetimestamp<='2017-12-31 23:45',]
g_hd <- ggplot(sub_dat5, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

g_ca
g_ld
g_hd

#Missing DO
missing_ca_do <- round(sum(is.na(sub_dat4$do_mgl))/nrow(sub_dat4)*100,2)
missing_ca_do #Looks like weird OW/MC mix? goes anoxic MENTION TO NELSON

missing_ld_do <- round(sum(is.na(sub_dat5$do_mgl))/nrow(sub_dat5)*100,2)
missing_ld_do #OW weird looking, but not missing much data

missing_hd_do <- round(sum(is.na(sub_dat6$do_mgl))/nrow(sub_dat6)*100,2)
missing_hd_do #OW Also same weirdness, lots missing too




#GTM (South of SAP)-----------------------------------------------------------------------------------------------
path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
sitename = 'gtmpiwq'
data_collected <- import_local(path, sitename, trace = FALSE)
pi <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
sitename = 'gtmfmwq'
data_collected <- import_local(path, sitename, trace = FALSE)
fm <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
sitename = 'gtmsswq'
data_collected <- import_local(path, sitename, trace = FALSE)
ss <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
sitename = 'gtmpcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
pc2 <- qaqc(data_collected)


sub_dat7<- pi[pi$datetimestamp>='2007-01-01 00:00' & pi$datetimestamp<='2017-12-31 23:45',]
g_pi <- ggplot(sub_dat7, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

sub_dat8<- fm[fm$datetimestamp>='2007-01-01 00:00' & fm$datetimestamp<='2017-12-31 23:45',]
g_fm <- ggplot(sub_dat8, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

sub_dat9<- ss[ss$datetimestamp>='2007-01-01 00:00' & ss$datetimestamp<='2017-12-31 23:45',]
g_ss <- ggplot(sub_dat9, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

sub_dat10<- pc2[pc2$datetimestamp>='2007-01-01 00:00' & pc2$datetimestamp<='2017-12-31 23:45',]
g_pc2 <- ggplot(sub_dat10, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

g_pi
g_fm
g_ss
g_pc2

#Missing DO
missing_pi_do <- round(sum(is.na(sub_dat7$do_mgl))/nrow(sub_dat7)*100,2)
missing_pi_do #OW

missing_fm_do <- round(sum(is.na(sub_dat8$do_mgl))/nrow(sub_dat8)*100,2)
missing_fm_do #OW hardly any missing data, weirdness though,  CHOOSE THIS ONE, BUT CHECK WITH NELSON. GTMFMWQ OPEN WATER

missing_ss_do <- round(sum(is.na(sub_dat9$do_mgl))/nrow(sub_dat9)*100,2)
missing_ss_do #OW, similar weirdness, highly populated area

missing_pc2_do <- round(sum(is.na(sub_dat10$do_mgl))/nrow(sub_dat10)*100,2)
missing_pc2_do #MC no weirdness, looks GOOD CHOSE THIS ONE GTMPCWQ MARSH CREEK



#ACE (North of SAP)---------------------------------------------------------------------------------------------------------
path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
sitename = 'acefcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
fc <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
sitename = 'acemcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
mc <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
sitename = 'acespwq'
data_collected <- import_local(path, sitename, trace = FALSE)
sp <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
sitename = 'acegpwq'
data_collected <- import_local(path, sitename, trace = FALSE)
gp <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
sitename = 'acejiwq'
data_collected <- import_local(path, sitename, trace = FALSE)
ji <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
sitename = 'acercwq'
data_collected <- import_local(path, sitename, trace = FALSE)
rc <- qaqc(data_collected)


sub_dat11<- fc[fc$datetimestamp>='2007-01-01 00:00' & fc$datetimestamp<='2017-12-31 23:45',]
g_fc <- ggplot(sub_dat11, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

sub_dat12<- mc[mc$datetimestamp>='2007-01-01 00:00' & mc$datetimestamp<='2017-12-31 23:45',]
g_mc <- ggplot(sub_dat12, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

sub_dat13<- sp[sp$datetimestamp>='2007-01-01 00:00' & sp$datetimestamp<='2017-12-31 23:45',]
g_sp <- ggplot(sub_dat13, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

sub_dat14<- gp[gp$datetimestamp>='2007-01-01 00:00' & gp$datetimestamp<='2017-12-31 23:45',]
g_gp <- ggplot(sub_dat14, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

sub_dat15<- ji[ji$datetimestamp>='2007-01-01 00:00' & ji$datetimestamp<='2017-12-31 23:45',]
g_ji <- ggplot(sub_dat15, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

sub_dat16<- rc[rc$datetimestamp>='2007-01-01 00:00' & rc$datetimestamp<='2017-12-31 23:45',]
g_rc <- ggplot(sub_dat16, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()

g_fc #meh
g_mc #meeh
g_sp #no
g_gp #no
g_ji #no
g_rc #no

missing_fc_do <- round(sum(is.na(sub_dat11$do_mgl))/nrow(sub_dat11)*100,2)
missing_fc_do 

missing_mc_do <- round(sum(is.na(sub_dat12$do_mgl))/nrow(sub_dat12)*100,2)
missing_mc_do 

missing_sp_do <- round(sum(is.na(sub_dat13$do_mgl))/nrow(sub_dat13)*100,2)
missing_sp_do 

missing_gp_do <- round(sum(is.na(sub_dat14$do_mgl))/nrow(sub_dat14)*100,2)
missing_gp_do 

missing_ji_do <- round(sum(is.na(sub_dat15$do_mgl))/nrow(sub_dat15)*100,2)
missing_ji_do

missing_rc_do <- round(sum(is.na(sub_dat16$do_mgl))/nrow(sub_dat16)*100,2)
missing_rc_do 

#--------------------------------------------------------------------------------------------------------------------
#WKB sites (east of GND)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\GulfofMexico"
sitename = 'wkbmbwq'
data_collected <- import_local(path, sitename, trace = FALSE)
mb <- qaqc(data_collected)

sub_dat17<- mb[mb$datetimestamp>='2007-01-01 00:00' & mb$datetimestamp<='2017-12-31 23:45',]
g_mb <- ggplot(sub_dat17, aes(x=datetimestamp, y=do_mgl)) +
  geom_point()
g_mb

missing_mb_do <- round(sum(is.na(sub_dat17$do_mgl))/nrow(sub_dat17)*100,2)
missing_mb_do 

