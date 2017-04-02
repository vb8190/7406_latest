# This code takes about 8 hours to run. 
# Once the adjacent locations are saved to file, 
# the variables take very little time to create.

import csv
import math
import datetime

print(datetime.datetime.now().time())

listingdata = "combined_renthop - new variables.csv"
datafileout = "renthop_adjacency.csv"

alllist = {}

with open(listingdata, encoding="utf-8") as datafile:
    data = csv.reader(datafile, delimiter = ",")
    next(datafile, None)
    for i in data:
        #i[27] is the interest level variable that only occurs for observations
        # from the training set.
        #i[25] and i[26] are latitude and longitude
        #i[0] is the observation number (0 to 124010)
        if i[27]:
            alllist[int(i[0])] = [float(i[25]),float(i[26]),"T"]
        else:
            alllist[int(i[0])] = [float(i[25]),float(i[26]),"F"]

#adj_matrix will contain adjacency info from the training set only
adj_matrix = {}
#adj_matrix_all will contain adjacency info for all observations
adj_matrix_all = {}

for i in range(len(alllist)):
    adj_matrix[i] = []
    adj_matrix_all[i] = []
    #key = j
    for key in alllist:
        #if i is not equal to j
        if key != i:
            lat_dif = alllist[key][0]-alllist[i][0]
            long_dif = alllist[key][1]-alllist[i][1]
            if lat_dif < 0:
                lat_bottom = alllist[i][0]
            else:
                lat_bottom = alllist[key][0]
            lat_middle = lat_bottom + lat_dif
            #converts differences to approximate miles to standardize distance
            # for lat and long changes and gets the Euclidean distance for each
            # obs_i to each obs_j
            distance = math.sqrt((69*lat_dif)**2 +
                (math.cos(math.radians(lat_middle))*69.1586*long_dif)**2)
            #storing distances and obs for the non-zero part of the matrix
            # where the number of adjacencies of concern = 30
            if len(adj_matrix_all[i]) < 30:
                adj_matrix_all[i] += [(distance, key)]
                adj_matrix_all[i].sort(reverse=True)
            elif distance < adj_matrix_all[i][0][0]:
                adj_matrix_all[i][0] = (distance, key)
                adj_matrix_all[i].sort(reverse=True)
            if len(adj_matrix[i]) < 30:
                if alllist[key][2] == "T":
                    adj_matrix[i] += [(distance, key)]
                    adj_matrix[i].sort(reverse=True)
            elif distance < adj_matrix[i][0][0]:
                if alllist[key][2] == "T":
                    adj_matrix[i][0] = (distance, key)
                    adj_matrix[i].sort(reverse=True)
    #monitor progress as the code runs
    if i % 2000 == 0:
        print(i)
        print(datetime.datetime.now().time())

headlist = ['observation', 'listing_id', 'adjacent_all','adjacent_train','interest_level']

#write non-zero matrix elements to file
with open(listingdata, encoding="utf-8") as datafile:
    data = csv.reader(datafile, delimiter = ",")
    next(datafile, None)
    with open(datafileout, 'w', encoding="utf-8", newline = '') as outfile:
        csvfile1 = csv.writer(outfile)
        csvfile1.writerow(headlist)
        for i in data:
            outline = [i[0],i[1],adj_matrix_all[int(i[0])],adj_matrix[int(i[0])],i[27]]
            csvfile1.writerow(outline)

print(datetime.datetime.now().time())
