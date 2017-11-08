#Sam Blackman
#10/30/17

#Importing Data by looking at the larger location files, so I don't have to individually go in and grab
#each Water Qaulity excel sheet


setwd(
  "C:/Users/sabla/Documents/Undergraduate_Research_Data/SWMPr_Data_For_R"
)

data <- list.files(path = ".", pattern = ".csv")

filtered <- which(grepl("wq", data))

new_data <- data[filtered]

View(new_data)

writeLines("Here is a list of all available water quality site data: \r\n Apalachicola, Florida (apa) \n Ashepoo Combahee Edisto Basin, SC (ace) \n Chesapeake Bay, MD (cbm) \n Chesapeake Bay, VA (cbv) \n Delaware, DE (del) \n Elkhorn Slough, CA (elk) \n Grand Bay, MS (gnd) \n Great Bay, NH (grb) \n Guana Tolomato Mantanzas, FL (gtm) \n Hudson River, NY (hud) \n Jacques Cousteau, NJ (jac) \n Jobos Bay, PR (job) \n Kachemak Bay, AK (kac) \n Lake Superior, WI (lks) \n Mission Aransas, TX (mar) \n Narragansett Bay, RI (nar) \n North Carolina, NC (noc) \n North Inlet-Winyah Bay, SC (niw) \n Old Woman Creek, OH (owc) \n Padilla Bay, WA (pdb) \n Rookery Bay, FL (rkb) \n San Francisco Bay, CA (sfb) \n Sapelo Island, GA (sap) \n South Slough, OR (sos) \n Tijuana River, CA (tjr) \n Waquoit Bay, MA (wqb) \n Weeks Bay, AL (wkb) \n Wells, ME (wel)")
site_code <- readline(prompt = "Enter the site code of the location you would like to examine:  ")

filtered <- which(grepl(site_code, new_data))
selected_data <- new_data[filtered]

View(selected_data)
