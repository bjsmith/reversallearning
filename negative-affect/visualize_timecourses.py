import re

#placeholder or not
#prereversal/postreversal
#error/correct
def visualize_timecourses(onsets,colsuffix):
    #add values corresponding to the value at the end of the name in each column
    add_vals_iteration=[int(re.findall("_[0-9]"+colsuffix+"$",s)[0][1:2]) for s in onsets.columns]
    add_vals_error=[10*int(re.search("_error_",s) is not None) for s in onsets.columns] #add 10 for error trials
    add_vals_postR=[20*int(re.search("PostR_",s) is not None) for s in onsets.columns] #add 20 for post-reversal trials
    add_vals_stimnum=[-53+10*int(re.findall("_i[0-9]+_",s)[0][2:4]) for s in onsets.columns]

    #add values to separate corret and error trials
    onsets_toplot=onsets*.8 + add_vals_iteration  + add_vals_error+add_vals_postR#+add_vals_stimnum

    #onsets_toplot.loc[:,[("placeholder" in x)==False for x in onsets_toplot.columns]].plot(legend=False)

    onsets_toplot.loc[:,[("_nonresponse_" in x)==False for x in onsets_toplot.columns]].plot(legend=False)

    onsets.loc[:,[("PreR_" in x) and ("PreR_" in x) and int(re.findall("_[0-9]"+colsuffix+"$",x)[0][1:2])==1 for x in onsets.columns]]
visualize_timecourses(onsets,colsuffix="")
visualize_timecourses(onsets_convolved.iloc[:,0:217],colsuffix="_c0")

