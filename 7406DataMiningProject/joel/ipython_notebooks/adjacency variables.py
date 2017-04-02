# This code takes only a couple of minutes to create all of the variables,
# but "location adjacency matrix.py" has to be run first to create the
# adjacent locations lists

import csv
import math
import datetime

print(datetime.datetime.now().time())

listingdata = "combined_renthop - new variables.csv"
adjacencydata = "renthop_adjacency.csv"
datafileout = "renthop_adjacency_vars.csv"

adjacent_all = {}
adjacent_train = {}

with open(adjacencydata, encoding = "utf-8") as datafile:
    data = csv.reader(datafile,delimiter = ",")
    next(datafile, None)
    for i in data:
        adjacent_all[int(i[0])] = eval(i[2])
        adjacent_train[int(i[0])] = eval(i[3])

for key in adjacent_all:
    adjacent_all[key].sort()

for key in adjacent_train:
    adjacent_train[key].sort()

interest_level = ['low','medium','high']

distance_adj = {}
days_between = {}
days_between_adj = {}
feature_count = {}
feature_count_adj = {}
descript_len = {}
descript_len_adj = {}
photo_count = {}
photo_count_adj = {}
log_price = {}
log_price_adj = {}
bedrooms = {}
bedrooms_adj = {}
bathrooms = {}
bathrooms_adj = {}
interest_code = {}
interest_adj = {}

with open(listingdata, encoding = "utf-8") as datafile:
    data = csv.reader(datafile, delimiter = ",")
    next(datafile, None)
    for i in data:
        days_between[int(i[0])] = int(i[9])
        feature_count[int(i[0])] = int(i[16])
        descript_len[int(i[0])] = int(i[18])
        photo_count[int(i[0])] = int(i[21])
        log_price[int(i[0])] = math.log(float(i[22]))
        bedrooms[int(i[0])] = int(i[23])
        bathrooms[int(i[0])] = float(i[24])
        if i[27]:
            interest_code[int(i[0])] = interest_level.index(i[27])+1

for key in adjacent_train:
    interest_adj[key] = []
    ia_sum = 0
    for i in range(len(adjacent_train[key])):
        ia_sum += interest_code[adjacent_train[key][i][1]]
        interest_adj[key] += [ia_sum]

for key in adjacent_all:
    distance_adj[key] = []
    days_between_adj[key] = []
    feature_count_adj[key] = []
    descript_len_adj[key] = []
    photo_count_adj[key] = []
    log_price_adj[key] = []
    bedrooms_adj[key] = []
    bathrooms_adj[key] = []
    d_sum = 0
    db_sum = 0
    fc_sum = 0
    dl_sum = 0
    pc_sum = 0
    lp_sum = 0
    bed_sum = 0
    bath_sum = 0
    for i in range(len(adjacent_all[key])):
        d_sum += adjacent_all[key][i][0]
        distance_adj[key] += [d_sum]
        db_sum += days_between[adjacent_all[key][i][1]]
        days_between_adj[key] += [db_sum]
        fc_sum += feature_count[adjacent_all[key][i][1]]
        feature_count_adj[key] += [fc_sum]
        dl_sum += descript_len[adjacent_all[key][i][1]]
        descript_len_adj[key] += [dl_sum]
        pc_sum += photo_count[adjacent_all[key][i][1]]
        photo_count_adj[key] += [pc_sum]
        lp_sum += log_price[adjacent_all[key][i][1]]
        log_price_adj[key] += [lp_sum]
        bed_sum += bedrooms[adjacent_all[key][i][1]]
        bedrooms_adj[key] += [bed_sum]
        bath_sum += bathrooms[adjacent_all[key][i][1]]
        bathrooms_adj[key] += [bath_sum]

head_prefix = ["interest_adj", "distance_adj", "days_between_adj",
    "feature_count_adj", "descript_len_adj", "photo_count_adj", "log_price_adj",
    "bedrooms_adj", "bathrooms_adj"]

headlist = ['observation', 'listing_id']

for x in head_prefix:
    for i in range(30):
        headlist += [x + "_" + str(i+1)]

headlist += ["interest_code"]

with open(listingdata, encoding="utf-8") as datafile:
    data = csv.reader(datafile, delimiter = ",")
    next(datafile, None)
    with open(datafileout, 'w', encoding="utf-8", newline = '') as outfile:
        csvfile1 = csv.writer(outfile)
        csvfile1.writerow(headlist)
        for i in data:
            outline = [i[0],i[1]]
            for x in head_prefix:
                active_var = eval(x + "[int(i[0])]")
                for xx in active_var:
                    outline += [xx]
            if i[27]:
                outline += [interest_code[int(i[0])]]
            csvfile1.writerow(outline)

print(datetime.datetime.now().time())

