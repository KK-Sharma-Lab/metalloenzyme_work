import pandas as pd

import numpy as np

import re

enz='/home/pratik/Downloads/metals_cofactors_expasy.xlsx'; #path to file having all metalloenzymes from database

eclist = pd.read_excel(enz) #pandas command to read excel file of metalloenzymes

eclist.head(5) #(not mendatory)to view the content stored in 'eclist' variable, shows top 5 entires of file

ec1=eclist.EC_num #assigning EC number column from metalloenzyme file to ec1 variable

ec2=eclist.Enzyme_name #assigning Enzyme names column to ec2 variable


samtsv= f"/home/pratik/Downloads/HMP2_healthy/{x}_humann2/{x}_ecs.tsv" # in here the 'f' in the begining of the string allows the integration og variable value in string using '{}', 
output= f"/home/pratik/Downloads/Metalloenzymes/{x}_me.xlsx" #so the input and output file address will change for each file iterations
    
sample = pd.read_csv(samtsv,delimiter='\t',encoding='utf-8') #code specifies, file is tab delimited by '\t' and encoding  

sample.columns=['Gene_family','Abundance_RPK'] #changes the names of columns sample for better referencing

sc1=sample.Gene_family #assigning column to variable

sc2=sample.Abundance_RPK #assigning variable to column

Result= pd.DataFrame(columns=['EC_Number','Enzyme_Name','Gene_Family','Abundance_RPK']) #creating a dataframe to store result
	#regex to match the EC_number '%a'taken enzyme_list and will match it with columns in sample file 
	#(%s will integrate the %a value in regex and \| will match the | sign after the EC number in gene family column))
for a,b in zip (ec1,ec2):
    for c,d in zip (sc1,sc2):
        p = re.search(r"^%s\|" %a, c) 
        if (a==c or p):
            Result = Result.append ({'EC_Number':a,'Enzyme_Name':b,'Gene_Family':c,'Abundance_RPK':d}, ignore_index=True)


Result.to_excel(output)


