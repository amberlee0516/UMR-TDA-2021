# -*- coding: utf-8 -*-
"""
Created on Tue Jun 29 14:55:35 2021

This method returns information about the interpolation
errors for each variable in the entire cleaned data set.

We are testing the following methods

Interpolation methods used here: Spatial - by year, by timecode
                                 Polynomial regression

@author: cashe
"""
import pandas as pd
import numpy as np
import os
from geopy import distance
import time
import pickle
from sklearn.model_selection import cross_val_score,train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import RobustScaler,PolynomialFeatures
import sklearn.metrics
pd.set_option('display.max_columns', None)

# useful functions and classes

# This class stores the latitude and longitude of a sample, and indicates 
# if this location has the desired variable we are estimating
class Location:
    def __init__(self,latitude,longitude,hasv,ID,value):
        self.ID = ID
        self.latitude = latitude
        self.longitude = longitude
        self.hasv = hasv
        self.value = value
        
    def __str__(self):
        return str(self.ID)

# Calculates the distance between 2 samples in km
def getdist(S1,S2):
    # radius of earth in km
    coords_1 = (S1.latitude, S1.longitude)
    coords_2 = (S2.latitude, S2.longitude)
    dist = distance.distance(coords_1, coords_2).km
    return dist


# PRE: all locations in the dataframe are
# unique
def DistanceMatrix(dataframe,variable):
    # numunique = len(dataframe["LOCATCD"].unique())
    # numlocations = dataframe.shape[0]
    # try:
    #     assert(numunique == numlocations), f"{numunique} unique locations but {numlocations} number of locations"
    
    # except AssertionError as msg:
    #     print(dataframe[dataframe["LOCATCD"].duplicated(keep=False)])
    #     print(msg)
        
    # the list of location objects
    locations = []
    # the list of indexes where the the row is located in the dataframe
    #indexes = []
    for index,row in dataframe.iterrows():
        # make a location object on this row
        if pd.isnull(row[variable]):
            hasv = False
        else:
            hasv = True
        locations.append(Location(row["LATITUDE"],row["LONGITUDE"],hasv,row["LOCATCD"],row[variable]))
        #indexes.append(index)
        
    matrix = pd.DataFrame(0,index=locations,columns=locations)
    for ci,column in enumerate(locations):
        for ri,row in enumerate(locations):
            if ri>ci:
                # compute distance between column and row
                dist = getdist(row,column)
            elif ci>ri:
                dist = matrix.iloc[ci,ri]
            # put this distance in the dataframe
            else:
                continue
            matrix.iloc[ri,ci] = dist
    return matrix

def changeVar(DM,dataframe,variable):
    locations = DM.index
    # loop through each location
    for i,loc in enumerate(locations):
        ID = loc.ID
        row = dataframe.loc[dataframe["LOCATCD"]==ID]
        #print(row)
        #print(row.shape)
        #print(row.loc[row.index[0],variable])
        #print(type(row[variable]))
        try:
            assert(row.shape[0]==1), "Multiple rows with same LOCATCD"
        except AssertionError as msg:
            print(dataframe[dataframe["LOCATCD"].duplicated(keep=False)])
            print(msg)
            
        # Pull value of desired variable
        val = row.loc[row.index[0],variable]
        if pd.isnull(val):
            locations[i].hasv = False
            locations[i].value = None
        else:
            locations[i].hasv = True
            locations[i].value = val
            
    DM.index = locations
    DM.columns = locations
        
def getclosest(numclosest,distancematrix,location):
    column = distancematrix.loc[:,location].copy()
    #print(type(distancematrix.index[0]))
    # Filter the locations that dont have the desired variable
    doesnthavev = []
    for i in range(len(column)):
        if not column.index[i].hasv:
            doesnthavev.append(column.index[i])
            
    # Get rid of locations that dont have the desired variable
    column.drop(doesnthavev,inplace = True)
    # Get rid of the location we are predicting for if it exists
    column.drop(location,inplace = True)
    #print(type(column))
    column.sort_values(inplace = True)
    
    return column.iloc[0:numclosest]

# Key: Location Codes that need predicting
# Value: List of tuples (locatcd,distance,value)
def makeDict(DM,numclosest,testing):
    # Loop through each location without a value for variable
    closestDict = {}
    for loc in DM.columns:
        if not loc.hasv or testing:
            # Get the closest locations to loc THAT ISN'T LOC
            closest = getclosest(numclosest,DM,loc)
            # The list of tuples that contain location id, the distance, and the value for variable
            tuples = []
            for i,dist in enumerate(closest):
                ID = closest.index[i].ID
                val = closest.index[i].value
                tuples.append((ID,dist,val))
            closestDict[loc.ID] = tuples
    return closestDict

def predict(tuples,numclosest = 2):
    loc2 = tuples[0]
    loc3 = tuples[1]
    d12 = loc2[1]
    val2 = loc2[2]
    d13 = loc3[1]
    val3 = loc3[2]
    
    if d12 == d13:
        return 0.5*d12+0.5*d13
    elif d12 == 0:
        return val2
    elif d13 == 0:
        return val3
    
    else:
        c2 = d12/(d12+d13)
        c3 = d13/(d12+d13)
        
        predicted = c2*val2+c3*val3
    
        return predicted

      
'''
data - the pandas dataframe that is ready to interpolate missing values
MUST HAVE "LATITUDE", "LONGITUDE","YEAR", "TIME CODE", "LOCATCD" columns

missing_vars - the list of column names (as strings) of the dataframe that we should attempt to fill in

numlocations - the number of locations used to predict the new value, default is 2 (currently the only option implemented)

RETURN - a dataframe with extra columns saying the predicted values of the missing_vars
'''
def linear_interpolate(data,missing_vars,numlocations = 2,testing = False,verbosity = 0):
    
    print("Building a new dataframe with predicted values")
    start_time = time.time()
    # Testing for duplicated locations if needed
    #s = qualdata_noprediction["LOCATCD"].duplicated(keep=False)
    # get the years and timecodes for this dataset
    # predictions can only be made if the point is in the same year and time code (what if we don't need to do this)
    years = data["YEAR"].unique()
    seasons = data["SEASON"].unique()
    pools = data["FLDNUM"].unique()
    data_prediction = pd.DataFrame()
    for pool in pools:
        for year in years:
            for season in seasons:
                if verbosity > 0:
                    print(f"Appending predicted data for {year}  {season}  FLDNUM {pool}")
                # curset is the current set of rows we are predicting for
                curset = data[(data["YEAR"]==year) & (data["SEASON"]==season) & (data["FLDNUM"]==pool)].copy()
                
                if verbosity > 1:
                    print("Size of this year and season:", curset.shape)
                
                # Boolean to indicate if variable in Distance matrix needs updating
                first = True
                for var in missing_vars:
                    newcolumn = "Predicted"+var
                    curset[newcolumn] = 0
                
                    #check to see if there are enough valid locations
                    # that can be used to predict
                    if not testing:
                        bad = bool((curset[var].notnull().sum()<numlocations))
                    else:
                        bad = bool((curset[var].notnull().sum()<numlocations+1))
                    
    
                    if(bad):
                        if verbosity > 2:
                            print("Less than "+str(numlocations)+" locations have "+var+" in this set, dropping rows without "+var)
                        curset = curset[curset[var].notnull()]
                        curset[newcolumn] = curset[var]
                        if verbosity > 2:
                            print("Current set is now ",curset.shape)
                    else:
                        if first:
                            if verbosity > 2:
                                print("Creating DM with ",var)
                            DM = DistanceMatrix(curset,var)
                            first = False
                        else:
                            if verbosity > 2:
                                print("Changing to ",var)
                            changeVar(DM,curset,var)
                            
                        # Returns a dictionary mapping each location code to a tuple with prediction information
                        Dict = makeDict(DM,numlocations,testing)
                        
                        #put in predicted variable
                        for index,row in curset.iterrows():
                            if pd.isnull(row[var]) or testing:
                                try:
                                    prediction = predict(Dict[row["LOCATCD"]])
                                    #print(curset.loc[index,newcolumn],prediction)
                                    curset.loc[index,newcolumn] = prediction
                                except ZeroDivisionError:
                                    print("Couldn't predict for ", str(row["LOCATCD"]))
                                    print(Dict[row["LOCATCD"]])
                                    curset.loc[index,newcolumn] = None
                            else:
                                curset.loc[index,newcolumn] = row[var]
    
                data_prediction = data_prediction.append(curset,ignore_index=True)  
    
    if verbosity > 0:
        print("Final data set size is ",data_prediction.shape)
    print(f"Interpolating took {(time.time()-start_time)/60} minutes")
    return data_prediction

# print("TESTING AT START")
# time.sleep(10)
# print("PRINTING AT END")

# Capture current time
global_start = time.time()

# Load the cleaned data
water_path = r"..\LTRM data\water_data_qfneg.csv"
water_data = pd.read_csv(water_path, low_memory = False)

continuous = ['TN','TP','TEMP','DO','TURB','COND','VEL','SS','WDP','CHLcal','SECCHI']
seasons = {3:"SPRING",4:"SPRING",5:"SPRING",6:"SUMMER",7:"SUMMER",8:"SUMMER",9:"FALL",10:"FALL",11:"FALL",12:"WINTER",1:"WINTER",2:"WINTER"}

print("Now adding a year column")
water_data["YEAR"] = pd.DatetimeIndex(water_data["DATE"]).year
print("Now adding a month column")
water_data["MONTH"] = pd.DatetimeIndex(water_data["DATE"]).month
print("Now adding a season column")
water_data["SEASON"] = water_data["MONTH"]
water_data = water_data.replace({"SEASON":seasons})

print("\n Water data")
print(water_data.columns)
print(water_data.shape)


print("Testing multivariate polynomial interpolation, using every other variable as a predictor besides target variable")
print("Filtering out all rows with missing data")
qualdata = water_data.dropna(axis=0, how='any', thresh=None, subset=continuous, inplace=False).copy()
print(qualdata.shape)
print("Filtering out colums that we dont need")
qualdata.drop(qualdata.columns.difference(continuous), 1, inplace=True)
print(qualdata.shape)
# The range of degree polynomials we will test for


# Building degree 1 polynomial for TP and TN
# Building degree 2 polynomial for vel
models = {"TN":1,"TP":1,"VEL":2}

for something in models:
    print(something)







degrees = range(1,5)
for var in continuous:
    print("\n-----------------------------------")
    print("Building model for ",var)
    # Get our predictor variables for this model
    predictors = continuous.copy()
    predictors.remove(var)
    
    X = np.array(qualdata[predictors])
    y = np.array(qualdata[var])
    
    # Good idea to standardize predictor attributes - assumes each variable has a decently normal distribution
    scaler = RobustScaler().fit(X)
    X_standard = scaler.transform(X)
    
    # Save the scaler for this model
    os.mkdir("Regression Models\\"+var)
    path = "Regression Models\\"+var+"\\"
    pickle.dump(scaler,open(path+"scaler.p", "wb" ))
    
    # Split data into training and test sets
    X_train, X_test, y_train, y_test = train_test_split(X_standard, y, train_size=0.8)
    
    # Save train and test sets
    pickle.dump(X_train,open(path+"X_train.p","wb"))
    pickle.dump(X_test,open(path+"X_test.p","wb"))
    pickle.dump(y_train,open(path+"y_train.p","wb"))
    pickle.dump(y_test,open(path+"y_test.p","wb"))


    
    
    valid_err = np.zeros(len(degrees))

    print('      CV Validation Error')
    for idx, d in enumerate(degrees):
        poly = PolynomialFeatures(d)
        poly.fit(X_train)
        lm = LinearRegression(fit_intercept=False)
        
        # Instead of fitting the model to the full training data, we'll use the cross_val_score function
        # in sklearn to do 5-fold cross-validation when fitting.
        scores = cross_val_score(lm, poly.transform(X_train), y_train, cv=5, scoring='neg_mean_squared_error')
        
        # scores is an array with k computed validation errors on the left-out folds
        # We average them together (and negate) to get a single score
        valid_err[idx] = -1 * np.mean(scores)
     
        print(f'd={d:2d}: {valid_err[idx]:15.4f}')
        
    best_d = degrees[np.argmin(valid_err)]
    print(f'Degree {best_d} polynomial has best cross-validation score')
    
    # Finally, we build the model, fit it to the full training data, and
    # estimate its out-of-sample performance by applying it to the test set
    best_poly = PolynomialFeatures(best_d)
    best_lm = LinearRegression(fit_intercept=False)
    best_lm.fit(best_poly.fit_transform(X_train), y_train)
    
    pickle.dump(best_lm,open(path+"best_model.p","wb"))
    pickle.dump(best_poly,open(path+"best_poly.p","wb"))

    
    # Estimate performance on test set:
    MSE = np.mean((y_test - best_lm.predict(best_poly.transform(X_test))) ** 2)
    RMSE = np.sqrt(MSE)
    MAE = np.mean(abs(y_test - best_lm.predict(best_poly.transform(X_test))))
    print(f'Degree {best_d} polynomial has RMSE = {RMSE:.5f}')
    print(f'Degree {best_d} polynomial has MAE = {MAE:.5f}')


print("\n\nTesting by year, by season spatial interpolation")
for var in continuous:
    print("\n-----------------------------------")
    print("Testing ",var)
    # Filter by locations that we already have for this variable
    water_test = water_data[water_data[var].notna()]
    water_test_interpolated = linear_interpolate(water_test,[var],testing=True)
    
    # Save the interpolated dataset
    path = "Interpolation_analysis_datasets\\"+var
    pickle.dump(water_test_interpolated,open(path+"_interpolated.p","wb"))
    
    
    # Get name of predicted column
    newcol = "Predicted"+var
    
    MAE = sklearn.metrics.mean_absolute_error(water_test_interpolated[var],water_test_interpolated[newcol])
    RMSE = sklearn.metrics.mean_squared_error(water_test_interpolated[var],water_test_interpolated[newcol],squared=False)
    print(f"The MAE for {var} is {MAE:8f}")
    print(f"The RMSE for {var} is {RMSE:8f}")
    
    # Make error column names
    error_col = var+" error"
    squared_error_col = var+" squared error"
    water_test_interpolated[error_col] = round(abs(water_test_interpolated[var] - water_test_interpolated[newcol]),6)
    water_test_interpolated[squared_error_col] = round((water_test_interpolated[var] - water_test_interpolated[newcol])**2,6)
    print(water_test_interpolated[error_col].describe())
    print(water_test_interpolated[squared_error_col].describe())
    


print(f"Entire analysis took {(time.time()-global_start)/60} minutes")

    



    


