# This python script requires to have installed the isimip-client API.
# See: https://github.com/ISI-MIP/isimip-client
# It can retrieve data stored here: https://data.isimip.org/ after applying a mask (to avoid downloading the whole world).

# Note 1: if you use RStudio, make sure that it is set to use the same Python interpreter 
# as the one in which you installed the API!

# Note 2: make sure that you have the same folder structure as the one assumed here: ./data/NC/ISIMIP_sources

# Note 3: on RStudio, the print messages are buffered... better to run the script in the terminal using python3:
# Simple example:
# >>> from download_nc_files_isimip import download_nc_data_isimip
# >>> download_nc_data_isimip(model="gfdl-esm4", ssp="ssp126")


import time
import os
from isimip_client.client import ISIMIPClient

def download_nc_data_isimip(model, scenario, crop=[27, 72, -13, 56], sim_round='ISIMIP3b', attempts = 10):

    c = ISIMIPClient()

    response = c.datasets(simulation_round=sim_round,
                          climate_forcing=model,
                          climate_scenario=scenario,
                          climate_variable='tas',
                          time_step="daily")
                          

    if not response["count"] == 1:
        print("input files not found for model =", model, "and scenario =", scenario)
        return -1
    
    dataset = response["results"][0]
    paths = [file['path'] for file in dataset['files']]

    #breakpoint() ## for debugging, requires python >= 3.7
    
    out_path = 'data/NC/ISIMIP_sources/' + model + '/' + scenario.upper() # output folder
    
    if not os.path.exists(out_path):
        os.makedirs(out_path)
    
    for i in range(attempts):
        
        # apply the mask (or check if jobs done upon recall):
        response = c.mask(paths=paths, bbox=crop, landonly=False) # bbox=[south, north, west, east]
                                                                  # landonly does not seem to work in combination of bbox...

        print("Current status of the job on the server:", response['status'], response['meta'])
        
        if response['status'] == 'finished':
            c.download(response['file_url'], path=out_path, validate=False)
            print("File saved (or already stored) at location:", out_path)
            break
        else:
            print("Waiting one minute for the server to have produced the files... ", i+1, "/", attempts)
            time.sleep(60)
        
        if i == attempts - 1:
            print("The sever did not successfully prepare all files in time.",
                  "Wait a little and rerun the function.",
                  "You may also increase the value the param attempts.\n",
                  "Error happened for model=", model, "and scenario=", scenario)
            return -2

    print("Done")
    return 0

# Dowloads needed for the project:

# from download_nc_files_isimip import download_nc_data_isimip

## FUTURE PROJECTIONS

# ssp_to_do = ['ssp126', 'ssp245', 'ssp370', 'ssp585']
# models_to_do = ['gfdl-esm4', 'ipsl-cm6a-lr', 'mpi-esm1-2-hr', 'mri-esm2-0', 'ukesm1-0-ll']
# projection_jobs = [download_nc_data_isimip(model=model, scenario=ssp, attempts = 100) for model in models_to_do for ssp in ssp_to_do]

## HISTORICAL DATA
# 
# historical_jobs = download_nc_data_isimip(model='gswp3-w5e5', scenario='obsclim', sim_round='ISIMIP3a', attempts = 100)
