import time
import multiprocessing 
import pandas as pd
import re



def multiprocessing_func(x):
    samtsv= f"/home/pratik/Downloads/HMP2_healthy/{x}_humann2/{x}_ecs.tsv"
    output= f"/home/pratik/Downloads/multi/{x}_me.xlsx"
    

    sample = pd.read_csv(samtsv,delimiter='\t',encoding='utf-8')

    sample.columns=['Gene_family','Abundance_RPK']

    sample.head(5)

    sc1=sample.Gene_family

    sc2=sample.Abundance_RPK

    Result= pd.DataFrame(columns=['EC_Number','Enzyme_Name','Gene_Family','Abundance_RPK'])

    for a,b in zip (ec1,ec2):
        for c,d in zip (sc1,sc2):
            p = re.search(r"^%s\|" %a, c)
            if (a==c or p):
                Result = Result.append ({'EC_Number':a,'Enzyme_Name':b,'Gene_Family':c,'Abundance_RPK':d}, ignore_index=True)


    Result.to_excel(output)  
    
    time.sleep(2)
    
if __name__ == '__main__':
    starttime = time.time()
    
    enz='/home/pratik/Downloads/metals_cofactors_expasy.xlsx';

    eclist = pd.read_excel(enz)

    ec1=eclist.EC_num

    ec2=eclist.Enzyme_name

    
    folder_list=pd.read_excel('/home/pratik/Documents/Folder_list2.xlsx')

    folder1=folder_list.Folder_id

    folder1.index = pd.MultiIndex.from_tuples([(x//104,x%104) for x in folder1.index]) #x//104 specifies the items per column so first look your data and then decide the number

    multi_col=folder1.unstack(0)

    multi_col.columns=['Folder_id_1','Folder_id_2','Folder_id_3','Folder_id_4','Folder_id_5'] #you should know how many columns you will get when you devide the data in group of 104

    folder1=multi_col.Folder_id_1 #I did not take folder_id_5 in this bcuz that column will have only 1 id and other will be "NNN", which will through error,

    folder2=multi_col.Folder_id_2 # the script will run,but it will keep throwing error "this file not found"

    folder3=multi_col.Folder_id_3 

    folder4=multi_col.Folder_id_4

    for a,b,c,d in zip(folder1,folder2,folder3,folder4):

        p1 = multiprocessing.Process(target=multiprocessing_func, args=(a,)) #you can increase the no of processors you are using by increasing this line
        p2 = multiprocessing.Process(target=multiprocessing_func, args=(b,))
        p3 = multiprocessing.Process(target=multiprocessing_func, args=(c,))
        p4 = multiprocessing.Process(target=multiprocessing_func, args=(d,))
        
        p1.start()
        p2.start()
        p3.start()
        p4.start()
        
        p1.join()
        p2.join()
        p3.join()
        p4.join()
        
        
    print('That took {} seconds'.format(time.time() - starttime))
