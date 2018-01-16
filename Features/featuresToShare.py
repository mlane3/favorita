"""
This is the program used for "submission1" on 12-7-17. It got a score of 1.555.
"""

import numpy as np
import datetime as dt
import csv
import pickle as pkl

directory = "/Users/lcmeeker/Desktop/Programming/Corporacion_Favorita/"

#### Functions that will be used for implementing the various models:
def getClusterNumber(storeNumber):
    """
    getClusterNumber returns the cluster number for "storeNumber"
    storeMetaDataFile is in format: store_nbr,city,state,type,cluster
    """
    storeMetaDataFile = open(directory+'data/stores.csv', 'rb')     # data in format: store_nbr,city,state,type,cluster
    for line in csv.reader(storeMetaDataFile):
        if line[0] == storeNumber:
            clusterNumber = line[-1]
            break
    return clusterNumber

def getStoresWithSameClusterNum(clusterNumber):
    """
    getStoresWithSameClusterNum iterates through the storeMetaDataFile.
    getStoresWithSameClusterNum returns a list of all stores with the same clusterNumber.
    storeMetaDataFile is in format: store_nbr,city,state,type,cluster
    """
    storeMetaDataFile = open(directory+'data/stores.csv', 'rb')     # data in format: store_nbr,city,state,type,cluster
    storesWithSameClusterNum = []
    for line in csv.reader(storeMetaDataFile):
        if line[-1] == clusterNumber:
            storesWithSameClusterNum.append(line[0])
    return storesWithSameClusterNum

def getSameStoreSubset(nparray, str_nbr):
    """
    getSameStoreSubset creates a subset of an item's data which only contains the item's data for a specific store location.
    getSameStoreSubset takes a 2-d nparray, a store number, and then returns a 2-d nparray containing only the item's data for the str_nbr parameter.
    The argument nparray and the nparray that is returned is in format: ['id' 'date' 'store_nbr' 'item_nbr' 'unit_sales' 'onpromotion']
    """
    subset = []
    if nparray[0][0] == np.string_('id'):   # check to see if "nparray's" first row is the column titles or not
        for row in nparray[1:]:         # start at row "1" due to row "0" being the column titles
            if row[2] == str_nbr:
                subset.append(row)
    else:
        for row in nparray:
            if row[2] == str_nbr:
                subset.append(row)
    subsetArray = np.array(subset)
    return subsetArray

def getClusterSubset(nparray, storeList):
    """
    This function has not been comleted....
    It will essentially do the same process as the function getSameStroreSubset,
    except that it will do it for all stores in the item's data ("nparray"), that have the same cluster number.
    """
    clusterSubset = []
    for store in storeList:
        storeData = getSameStoreSubset(nparray, store)
        clusterSubset.append(storeData)
    print clusterSubset

def getDates(date, numPreviousDays, numFutureDays):
    """
    getDates takes a date, numPreviousDays, and numFutureDays, where date is a type numpy.string in format year-month-day,
    and numPreviousDays/numFutureDays are ints.
    getDates returns a nparray of dates of type numpy.string in range [date - numPreviousDays, date + numFutureDays].
    """
    parseDate = date.split('-')
    yr = int(parseDate[0])
    month = int(parseDate[1])
    day = int(parseDate[2])
    dtDate = dt.date(yr, month, day)
    dateOrdinal = dt.date.toordinal(dtDate)
    previousDateOrdinal = dateOrdinal - (numPreviousDays+1)
    futureDateOrdinal = dateOrdinal + numFutureDays

    dates = []
    for num in range(previousDateOrdinal, futureDateOrdinal):
        date = dt.date.fromordinal(num)
        year = str(date.year)
        if date.month < 10:
            month = '0'+str(date.month)
        else:
            month = str(date.month)
        if date.day < 10:
            day = '0'+str(date.day)
        else:
            day = str(date.day)
        dateNpString = np.string_(year+'-'+month+'-'+day)
        dates.append(dateNpString)
    arrayDates = np.array(dates)
    return arrayDates


def getAllDataAVG(nparray):
    """
    getAllDataAVG takes an item's nparray and returns the average for all units sold in the array (type: numpy.float64).
    array is in format: ['id' 'date' 'store_nbr' 'item_nbr' 'unit_sales' 'onpromotion']
    """
    unitsSold = []
    for row in nparray:
        unitsSold.append(np.float64(row[4]))
    arrayToAVG = np.array(unitsSold)
    avgUnitsSold = np.mean(arrayToAVG)
    return avgUnitsSold

#### This Function may or may not be complete (can't remember if I ever finished yet):
def getAVGinDateRange(nparray, date, numPreviousDays, numFutureDays):
    dateRange = getDates(date, numPreviousDays, numFutureDays)
    listInDateRange = []
    for date in dateRange:
        for row in nparray:
            if date == row[1]:
                listInDateRange.append(row)
    arrayInDateRange = np.array(listInDateRange)
    arrayToAVG = getSameStoreSubset(arrayInDateRange, nparray[0][2])

    return getAllDataAVG(arrayToAVG)


#### Beginning of Code for running model/s: ####

# Open the file containing the dictionary of all items that we have data for:
itemDic = pkl.load(open(directory+"data/itemDic.pkl", 'rb'))  # "itemDic" is a dictionary with item numbers as the key and number of rows for the item in train.csv as the value.
itemsToTestFile = open(directory+"data/test.csv", 'rb') #  This is the file we test our models on, it is in format: id,date,store_nbr,item_nbr,onpromotion


#### To run the particular model I submitted (12-7), we iterate through the "test.csv" file.
#### For each iteration the average number of units sold for each item is written to the submission file regardless of any other attribute for that row in the test file.
#### The methodolgy is to quickly and easily build more complex "features" using the functions above (and functions yet to be).
#### For instance the next step could be to take the average for only the item's data that has the same store number as
#### the particular row being tested, instead of taking the average for all the item's data regardless of store (as done below).
#### Then one could pay attention to dates - start only using particular dates surounding the date of row being tested, etc. etc.

# count is used to have a print out, so that you can "see" the program working (only prints every 25000 time through test.csv) 
count = 1
submissionCSV = open(directory+'/Data/submissions/'+'submission1.csv', 'wb')
writer = csv.writer(submissionCSV, dialect='excel')
for row in csv.reader(itemsToTestFile):
    if count%25000 == 0:
        print count
    # all of the individual item files start with the train.csv header: ['id' 'date' 'store_nbr' 'item_nbr' 'unit_sales' 'onpromotion'].
    # So just add the header to the submission file the first iteration.
    if count == 1:
        count += 1
        nextLine = ['id', 'unit_sales']
        writer.writerow(nextLine)
        continue
    else:
        # for each row we test, put each column into a variable:
        test_i_d = row[0]
        test_date = row[1]
        test_store_nbr = row[2]
        test_item_nbr = row[3]
        test_onPromo = row[4]
        # open the item's data file for the row we are testing:
        if test_item_nbr in itemDic:
            count += 1
            itemDataFile = np.load(directory+"data/items/"+test_item_nbr+".npy")    # itemDataFile has format:  ['id' 'date' 'store_nbr' 'item_nbr' 'unit_sales' 'onpromotion']
            #### Run model here ####
            avgUnitsSold = getAllDataAVG(itemDataFile[1:])
            unitsSold = int(avgUnitsSold) + 1

        #### Darren is working on what to do if there is an item not contained in itemDic (not in the train.csv file), for now I made up a number (it wasn't completely arbitrary):
        else:
            unitsSold = 8.70036
        #### add prediction to submission file:
        nextLine = [str(test_i_d), str(unitsSold)]
        writer.writerow(nextLine)
        #### break statement is here if you want to test what you are working on, otherwise program woud iterate over 3 million plus times.
        # if count == 10:
        #     break
print 'Done'
#### count should be 3,318,626 if you iterate through all of "test.csv":
print count
