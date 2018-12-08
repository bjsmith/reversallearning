import numpy as np
import pandas as pd
from scipy.stats import ttest_1samp, t, norm



#copied from Luke Chang's NeuroLearn Data Classes, this is adapted to process a pandas adat table instaed of a Brain_Data
def regress_rois(X,data):
    """ run vectorized OLS regression across voxels.

    Returns:
        out: dictionary of regression statistics in Brain_Data instances
            {'beta','t','p','df','residual'}

    """

    if not isinstance(X, pd.DataFrame):
        raise ValueError('Make sure self.X is a pandas DataFrame.')

    if X.empty:
        raise ValueError('Make sure self.X is not empty.')

    if data.shape[0] != X.shape[0]:
        raise ValueError("X does not match the correct size of "
                         "self.data")

    b = np.dot(np.linalg.pinv(X), data)
    res = data - np.dot(X, b)
    sigma = np.std(res, axis=0, ddof=X.shape[1])
    stderr = np.dot(np.matrix(np.diagonal(np.linalg.inv(np.dot(X.T,
                                                               X))) ** .5).T, np.matrix(sigma))


    #X=onsets_convolved
    #data=sub_r_m_roi_dt
    b = np.dot(np.linalg.pinv(X), data)
    res = data - np.dot(X, b)
    sigma = np.std(res, axis=0, ddof=X.shape[1])
    stderr = np.dot(np.matrix(np.diagonal(np.linalg.inv(np.dot(X.T,
                                                               X))) ** .5).T, np.matrix(sigma))

    b_out_data = b
    #t_out = deepcopy(self)
    t_out_data = b / stderr
    df = np.array([X.shape[0] - X.shape[1]] * data.shape[1])
    #p_out = deepcopy(self)
    p_out_data = 2 * (1 - t.cdf(np.abs(t_out_data), df))

    # Might want to not output this info
    #df_out = deepcopy(self)
    df_out_data = df
    #sigma_out = deepcopy(self)
    sigma_out_data = sigma
    #res_out = deepcopy(self)
    res_out_data = res

    return {'beta_vals': b_out_data, 't_vals': t_out_data, 'p_vals': p_out_data, 'df_vals': df_out_data,
            'sigma_vals': sigma_out_data, 'residual_vals': res_out_data}