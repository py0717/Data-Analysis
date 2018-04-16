import json
import pandas as pd
from pandas import DataFrame
from collections import Counter
import csv

def convert_json_to_list(filepath):
    """ inputs file path of json file, reads it then returns data in list form """
    json_file = open(filepath, 'r')
    data = [json.loads(line) for line in json_file]

    return data

def friends_and_average(data):
    """ inputs list of dictionaries and returns a dataframe with only
        number of friends and average rating for each user  """
    friends = []
    average = []

    for i in data:
        friends.append(len(i['friends']))
        average.append(i['average_stars'])

    data = DataFrame({'friends' : friends,
                     'average_stars' : average})
        
    return data        
    
# read json file and obtain dataframe with only relevant columns
lst = convert_json_to_list('C:/Users/my computer/Documents/yelp_academic_dataset_user.json')
df = friends_and_average(lst)

# write to csv to import into R
df.to_csv('C:/Users/my computer/Documents/out.csv', encoding='utf-8', index=False)
    
    
