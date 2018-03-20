import pandas
from pandas import *
import scipy.io
import re

#this file compares the FSL output with the pandas convolution.
#at teh end you can see that they all line up beautifully.

#now test.

sub113r1_fsl=pandas.read_table(
    "/Users/benjaminsmith/GDrive/joint-modeling/reversal-learning/behavioral-analysis/data/rlPainNPSTesting/sub113_punish_r1/design.mat.txt",
    header=None,index_col=None)

#only 1st, 3rd, etc columns
sub113r1_fsl_agg=(sub113r1_fsl).iloc[:,::2].iloc[:,0:12]
sub113r1_fsl_staggered=(sub113r1_fsl).iloc[:,::2].iloc[:,0:12]+sub113r1_fsl.columns/2
sub113r1_fsl_staggered.plot(title="from fsl")

#aggregate the onsets convolved that I have generated here through neurolearn.

colsuffix="_c0"

def aggregate_rl_cols(c):

    agg_col_name=""
    if re.search("PostR_", c) is not None:
        agg_col_name="PostReversal"
    else:
        agg_col_name += "PreReversal"

    if re.search("_error_", c) is not None:
        agg_col_name += "Error"
    else:
        agg_col_name += "Correct"

    agg_col_name += "_"+ str(re.findall("_[0-9]" + colsuffix + "$", c)[0][1:2])

    #agg_col_name += "_" + str(re.findall("_i[0-9]+_", c)[0][2:4])
    return agg_col_name


def aggregate_rl_cols_as_in_fsl(c):

    agg_col_name=""
    #grouping error cols together
    if re.search("_error_", c) is not None:
        agg_col_name = "E" + agg_col_name
    else:
        #if not an error column, then get the number and mark by that number.
        agg_col_name += str(re.findall("_[0-9]" + colsuffix + "$", c)[0][1:2])

    #First PreRev Correct
    #then PreRev Error
    #then PostRev Error
    #then PostRev Correct

    if re.search("_error_", c) is not None:
        if re.search("PostR_", c) is not None:
            agg_col_name = "C" + agg_col_name + "PostReversal"
        else:
            agg_col_name = "A" + agg_col_name + "PreReversal"

        agg_col_name = "B" + agg_col_name + "Error"
    else:
        agg_col_name = agg_col_name + "Correct"
        if re.search("PostR_", c) is not None:
            agg_col_name = "C" + agg_col_name + "PostReversal"
        else:
            agg_col_name = "A" + agg_col_name + "PreReversal"






    #agg_col_name += "_" + str(re.findall("_i[0-9]+_", c)[0][2:4])
    return agg_col_name

onsets_convolved_ex_lowpass=onsets_convolved.iloc[:,0:217]
onsets_convolved_agg=onsets_convolved_ex_lowpass.iloc[:,[re.search("_nonresponse_",c) is None for c in onsets_convolved_ex_lowpass.columns]].groupby(aggregate_rl_cols_as_in_fsl,axis=1).sum()


#remove 6-8 and non-response tirals.

neurolearn_onsets=onsets_convolved_agg.loc[:,[re.search("[6-8]",x) is None for x in onsets_convolved_agg.columns]]


neurolearn_onsets_staggered=neurolearn_onsets+ list(range(0,neurolearn_onsets.shape[1]))


neurolearn_onsets_staggered.plot(title="from neurolearn")

sub113r1_fsl_staggered.columns="fsl_"+neurolearn_onsets.columns

pandas.concat([sub113r1_fsl_staggered,neurolearn_onsets_staggered]).plot(legend=False)
#a couple that don't overlap; those will be the non-response cols.