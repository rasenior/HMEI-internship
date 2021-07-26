# -*- coding: utf-8 -*-
"""
Created on Thu Sep 26 13:36:16 2019

@author: rsenior
"""

import ee
import ee.mapclient
import rangeStats as rs
import pandas as pd
ee.Initialize()

# Read in IUCN data
iucndat = pd.read_csv('../../conservation-actions/data/all_iucn_dat.csv', low_memory=False)

# Filter to terrestrial, threatened species NOT possibly extinct
subdat = iucndat[
    ((iucndat['systems']=='Terrestrial') | 
    (iucndat['systems']=='Terrestrial|Freshwater (=Inland waters)')) &
    (iucndat['redlistCategory'].isin(["Critically Endangered", "Endangered" , "Vulnerable"])) &
    (iucndat['possiblyExtinct']== False) & 
    (iucndat['possiblyExtinctInTheWild']== False)]

# List all classes considered
classes = ['amphibia','magnoliopsida','reptilia','aves','mammalia']
taxaName = classes[4]
sppAll = list(set(subdat[subdat['className']==taxaName].scientificName.to_list()))
  
# All species ranges
sppRanges = ee.FeatureCollection(('users/rasenior/IUCN-range/2020/' + taxaName + '_2020'))
sppBbox = ee.FeatureCollection(('users/rasenior/IUCN-range/2020/bbox/' + taxaName + '_bbox'))
rangeSpp = sppRanges.aggregate_array('binomial').distinct().getInfo()
bboxSpp = sppBbox.aggregate_array('species').distinct().getInfo()

# Habitat preference data
habitatPrefs = ee.FeatureCollection('users/rasenior/IUCN-data/habitat_prefs')
prefsSpp = habitatPrefs.aggregate_array('scientificName').distinct().getInfo()

# Subset to species with range, bbox & habitat prefs data
spp = ee.List([
    x for x in sppAll if ((x in rangeSpp) 
                          and (x in bboxSpp) 
                          and (x in prefsSpp))]).sort()

# WDPA
wdpaPath = 'users/rasenior/WDPA/wdpa_2021-05-19' # May 2020 WDPA

# Global Islands Database
gidPath = 'users/rasenior/islands/GID'

# Projection & scale
wkt_mollweide = '\
    PROJCS["World_Mollweide", \
    GEOGCS["GCS_WGS_1984", \
        DATUM["WGS_1984", \
            SPHEROID["WGS_1984",6378137,298.257223563]], \
        PRIMEM["Greenwich",0], \
        UNIT["Degree",0.017453292519943295]], \
    PROJECTION["Mollweide"], \
    PARAMETER["False_Easting",0], \
    PARAMETER["False_Northing",0], \
    PARAMETER["Central_Meridian",0], \
    UNIT["Meter",1], \
    AUTHORITY["EPSG","54009"]]'
proj = ee.Projection(wkt_mollweide) # Mollweide projection
spScale = ee.Number(2e3) # resolution for RL thresholds

# Elevation data
elevpath = 'USGS/SRTMGL1_003' # SRTM v3

# Land cover data -------------------------------------------------------------

# Land cover data
lc_jung = (ee.ImageCollection('users/rasenior/LandCover/lvl2_frac_1km_ver004')
  # Convert to Image with bands corresponding to habitat types
  .toBands())
# Rename bands to match habitat preference data
old_bands = lc_jung.bandNames()
new_bands = (old_bands.map(lambda b: ee.String(b)
                           .replace('lvl2_', '')
                           .replace('_b1', '')))
lcImg = (lc_jung
  .select(old_bands)
  .rename(new_bands)
  # Force fractional values back to ranging from 0-1
  .divide(1000)
  # Set the year
  .set('year', 'yr2015')
  # Ensure nothing is masked
  .unmask())
  
lcYrs = ee.List([2015])
lcYrsStr = lcYrs.map(lambda i: ee.String("yr").cat(ee.Number(i).int().format()))
lcType = 'fraction'
lcField = 'jung'

# Convert to collection with one image per year
lc = ee.ImageCollection(lcImg)

# Habitat prefs ---------------------------------------------------------------

# Habitat importance
major = ee.List(["no","yes","NA"])
suitability = ee.List(["yes","NA"]) # exclude marginal
season = ee.List(['resident', 'breeding', 'non-breeding','NA']) # exclude passage

# Get EOO ---------------------------------------------------------------------
spplen = spp.size().getInfo()
eooColl = []
for i in range(spplen):
    sp = spp.get(i)
    print('Getting EOO for ' + str(i + 1) + ' of ' + str(spplen + 1))
    eoo = rs.EOO(sppRanges, sppBbox, sp, proj, spScale).set('species',sp)
    # Append to list
    eooColl.append(eoo)

# Get AOH stats ---------------------------------------------------------------
# How to restrict AOH by seasonality depends on purpose
# - if for RL thresholds comparison, use EOO season only
# - if for full global distribution, use all relevant seasons 

aohColl = []
bboxes = []
for i in range(spplen):
    sp = spp.get(i)
    print('Getting AOH for species ' + str(i + 1) + ' of ' + str(spplen + 1))
    seasonEOO = ee.Number(eooColl[i].get('eooSeason'))
    spSeason = ee.List([season.get(seasonEOO.subtract(1))]).add('NA')
    aoh = rs.getAOH(sp,sppRanges,sppBbox,
                    lc,lcField,lcType,lcYrsStr,
                    elevpath,habitatPrefs,
                    spSeason,
                    suitability,major).set('species',sp)
    # Update mask for export
    bbox = ee.Feature(sppBbox.filter(ee.Filter.eq('species', sp)).first())
    aoh = aoh.unmask(0).clip(bbox)
    # Append to list
    aohColl.append(aoh)
    bboxes.append(bbox)    

aohStats = []    
for i in range(spplen):
    sp = spp.get(i)
    print('Calculating AOH stats for species ' + str(i + 1) + ' of ' + str(spplen + 1))
    # Get AOH stats
    aohStats_i = (
        rs.AOHstats(sp,sppBbox,aohColl[i],wdpaPath,proj,spScale))
    # Append to list
    aohStats.append(aohStats_i)
    
    # Export
    aohtask = ee.batch.Export.table.toDrive(**{
        'collection': aohStats[i],
        'folder': 'jung2015_RL_' + taxaName,
        'fileNamePrefix': sp.getInfo().replace(" ", "_"),
        'fileFormat': 'CSV',
        'description': sp.getInfo().replace(" ", "-")})
    aohtask.start()
    
    
