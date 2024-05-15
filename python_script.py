## Libraries
import pandas as pd
from pyrosm import get_data, OSM
import geopandas as gpd
from shapely.geometry import Point


## Do parallel processing
import multiprocessing as mp
mp.Pool(mp.cpu_count())


## Input data
location_fp = "INPUTDATA.csv"
location_df = pd.read_csv(location_fp, sep = ";")
location_df.head()


## Get the locations for each province
## To get bounding box for the whole country, it takes very long time even after do parallel processing. Therefore, bounding box is determined for each province
## Use "initialize the OSM parser object" and "get the boundary" parts for each province, for example Drenthe
## After bounding box for all provinces are defined, combine all datasets
## The pbf file can be downloaded in this link 'https://download.geofabrik.de/europe/netherlands.html'

# Initialize the OSM parser object
dr = "drenthe-latest.osm.pbf"
#fl = "flevoland-latest.osm.pbf"
#fr = "friesland-latest.osm.pbf" 
#gl = "gelderland-latest.osm.pbf"
#gr = "groningen-latest.osm.pbf"
#li = "limburg-latest.osm.pbf"
#nb = "noord-brabant-latest.osm.pbf"
#nh = "noord-holland-latest.osm.pbf"
#ov = "overijssel-latest.osm.pbf"
#ut = "utrecht-latest.osm.pbf"
#ze = "zeeland-latest.osm.pbf"
#zh = "zuid-holland-latest.osm.pbf"
#nl = "netherlands-latest.osm.pbf"
osm_data = OSM(dr) #edit based on the province defined before

# Get bounding box (using pyrosm)
boundaries = osm_data.get_boundaries()

# Get the boundary
province_boundary = boundaries[boundaries["operator"]=="Provincie Drenthe"]["geometry"]
#province_boundary = boundaries[boundaries["operator"]=="Provincie Flevoland"]["geometry"]
#province_boundary = boundaries[boundaries["operator"]=="Provincie Fryslân"]["geometry"]
#province_boundary = boundaries[boundaries["operator"]=="Provincie Gelderland"]["geometry"]
#province_boundary = boundaries[boundaries["operator"]=="Provincie Groningen"]["geometry"]
#province_boundary = boundaries[boundaries["operator"]=="Provincie Limburg"]["geometry"]
#province_boundary = boundaries[boundaries["operator"]=="Provincie Noord-Brabant"]["geometry"]
#province_boundary = boundaries[boundaries["operator"]=="Provincie Noord-Holland"]["geometry"]
#province_boundary = boundaries[boundaries["operator"]=="Provincie Overijssel"]["geometry"]
#province_boundary = boundaries[boundaries["operator"]=="Provincie Utrecht"]["geometry"]
#province_boundary = boundaries[boundaries["operator"]=="Provincie Zeeland"]["geometry"]
#province_boundary = boundaries[boundaries["operator"]=="Provincie Zuid-Holland"]["geometry"]
#type(province_boundary)

# Filter locations in the current province
points = (Point(row["longitude"], row["latitude"]) for idx, row in location_df.iterrows())

in_boundary = [province_boundary.contains(p).tolist()[0] for p in points]
print(in_boundary)

location_df = location_df[in_boundary]
print(location_df)


## Add tags to bounding box for each province
# Apply no filter (for all POIs)
osm_poi = osm_data.get_pois()  

# Specify POI types  
osm_poi_schools = osm_data.get_pois(custom_filter={"amenity": ["school"]}) # schools 
osm_poi_offices = osm_data.get_pois(custom_filter={"office": True}) # offices 
osm_poi_pub = osm_data.get_pois(custom_filter={"amenity": ["pub"]}) # pub
osm_poi_college = osm_data.get_pois(custom_filter={"amenity": ["college"]}) # college
osm_poi_nursing = osm_data.get_pois(custom_filter={"amenity": ["nursing_home"]}) # nursing_home
osm_poi_aerodrome = osm_data.get_pois(custom_filter={"aeroway": ["aerodrome"]}) # aerodrome
osm_poi_bar = osm_data.get_pois(custom_filter={"amenity": ["bar"]}) # bar
osm_poi_cafe = osm_data.get_pois(custom_filter={"amenity": ["cafe"]}) # cafe
osm_poi_fastfood = osm_data.get_pois(custom_filter={"amenity": ["fast_food"]}) # fastfood
osm_poi_restaurant = osm_data.get_pois(custom_filter={"amenity": ["restaurant"]}) # restaurant
osm_poi_university = osm_data.get_pois(custom_filter={"amenity": ["university"]}) # university
osm_poi_bus_st = osm_data.get_pois(custom_filter={"amenity": ["bus_station"]}) # bus-station
osm_poi_parking = osm_data.get_pois(custom_filter={"amenity": ["parking"]}) # parking
osm_poi_bank = osm_data.get_pois(custom_filter={"amenity": ["bank"]}) # bank
osm_poi_dentist = osm_data.get_pois(custom_filter={"amenity": ["dentist"]}) # dentist
osm_poi_doctors = osm_data.get_pois(custom_filter={"amenity": ["doctors"]}) # doctors
osm_poi_hospital = osm_data.get_pois(custom_filter={"amenity": ["hospital"]}) # hospital
osm_poi_pharmacy = osm_data.get_pois(custom_filter={"amenity": ["pharmacy"]}) # pharmacy
osm_poi_veterinary = osm_data.get_pois(custom_filter={"amenity": ["veterinary"]}) # veterinary
osm_poi_cinema = osm_data.get_pois(custom_filter={"amenity": ["cinema"]}) # cinema
osm_poi_police = osm_data.get_pois(custom_filter={"amenity": ["police"]}) # police
osm_poi_apartment = osm_data.get_pois(custom_filter={"building": ["apartments"]}) # apartments
osm_poi_hotel = osm_data.get_pois(custom_filter={"building": ["hotel"]}) # hotel
osm_poi_house = osm_data.get_pois(custom_filter={"building": ["house"]}) # house
osm_poi_residential = osm_data.get_pois(custom_filter={"building": ["residential"]}) # residential
osm_poi_office = osm_data.get_pois(custom_filter={"building": ["office"]}) # office
osm_poi_motorway = osm_data.get_pois(custom_filter={"highway": ["motorway"]}) # motorway
osm_poi_trunk = osm_data.get_pois(custom_filter={"highway": ["trunk"]}) # trunk
osm_poi_primary = osm_data.get_pois(custom_filter={"highway": ["primary"]}) # primary
osm_poi_secondary = osm_data.get_pois(custom_filter={"highway": ["secondary"]}) # secondary
osm_poi_tertiary = osm_data.get_pois(custom_filter={"highway": ["tertiary"]}) # tertiary
osm_poi_pedestrian = osm_data.get_pois(custom_filter={"highway": ["pedestrian"]}) # pedestrian
osm_poi_busway = osm_data.get_pois(custom_filter={"highway": ["busway"]}) # busway
osm_poi_footway = osm_data.get_pois(custom_filter={"highway": ["footway"]}) # footway
osm_poi_path = osm_data.get_pois(custom_filter={"highway": ["path"]}) # path
osm_poi_sidewalk = osm_data.get_pois(custom_filter={"footway": ["sidewalk"]}) # sidewalk
osm_poi_cycleway = osm_data.get_pois(custom_filter={"highway": ["cycleway"]}) # cycleway
osm_poi_lane = osm_data.get_pois(custom_filter={"cycleway": ["lane"]}) # lane
osm_poi_bus_stop = osm_data.get_pois(custom_filter={"highway": ["bus_stop"]}) # bus-stop
osm_poi_platform = osm_data.get_pois(custom_filter={"highway": ["platform"]}) # platform
osm_poi_commercial_land = osm_data.get_pois(custom_filter={"landuse": ["commercial"]}) # commercial-land
osm_poi_edu_land = osm_data.get_pois(custom_filter={"landuse": ["education"]}) # edu-land
osm_poi_industrial_land = osm_data.get_pois(custom_filter={"landuse": ["industrial"]}) # industrial-land
osm_poi_residential_land = osm_data.get_pois(custom_filter={"landuse": ["residential"]}) # residential-land
osm_poi_retail_land = osm_data.get_pois(custom_filter={"landuse": ["retail"]}) # retail-land
osm_poi_railway_land = osm_data.get_pois(custom_filter={"landuse": ["railway"]}) # railway-land
osm_poi_fitness = osm_data.get_pois(custom_filter={"leisure": ["fitness_centre"]}) # fitness
osm_poi_sport_center = osm_data.get_pois(custom_filter={"leisure": ["sports_centre"]}) # sports
osm_poi_swim = osm_data.get_pois(custom_filter={"leisure": ["swimming_pool"]}) # swiwmming pool
osm_poi_shops = osm_data.get_pois(custom_filter={"shop": True}) # all kinds of shops
osm_poi_routes = osm_data.get_pois(custom_filter={"route": True}) # all kinds of routes
osm_poi_sports = osm_data.get_pois(custom_filter={"sport": True}) # all kinds of sports 


## Define radius for bounding box of each tag
## Four radiuses are used (25m, 35m, 50m, and 200m)

# Define the radius in meters
radius = 25 #change this radius to 35, 50, or 200 later

# Function to filter POIs within a certain radius
def filter_pois_by_radius(poi_df, location, radius):
    # perform this in RD New (EPSG:28992) so the radius is in meters
    
    # Create a circular buffer around the location in RD New (EPSG:28992)
    buffer = location.buffer(radius)
    # Reproject POI dataframe to RD New (EPSG:28992)
    poi_df = poi_df.to_crs(epsg=28992)
    # Filter POIs within the buffer
    return poi_df[poi_df.geometry.intersects(buffer)]


## Create an empty vector for each tag
n_pois = []
n_schools = []
n_offices = []
n_pub = []
n_college = []
n_nursing = []
n_aerodrome = []
n_bar = []
n_cafe = []
n_fastfood = []
n_restaurant = []
n_university = []
n_bus_st = []
n_parking = []
n_bank = []
n_dentist = []
n_doctors = []
n_hospital = []
n_pharmacy = []
n_veterinary = []
n_cinema = []
n_police = []
n_apartment = []
n_hotel = []
n_house = []
n_residential = []
n_office = []
n_motorway = []
n_trunk = []
n_primary = []
n_secondary = []
n_tertiary = []
n_pedestrian = []
n_busway = []
n_footway = []
n_path = []
n_sidewalk = []
n_cycleway = []
n_lane = []
n_bus_stop = []
n_platform = []
n_commercial_land = []
n_edu_land = []
n_industrial_land = []
n_residential_land = []
n_retail_land = []
n_railway_land = []
n_fitness = []
n_sport_centre = []
n_swim = []
n_shops = []
n_routes = []
n_sports = []


## Count the number of POIs of specific tags
# Process each location
for index, row in location_df.iterrows():
    # Define the location
    location = Point(row['longitude'], row['latitude'])

    # Transform the point to RD New (EPSG:28992)
    location_transformed = gpd.GeoSeries([location], crs='EPSG:4326').to_crs(epsg=28992)[0]
    
    # Extract nearby poi for the selected location
    #nearby_poi = filter_pois_by_radius(osm_poi, location_transformed, radius)
    
    # Count the number of poi of each type we are interested in 
    # Save these numbers in the arrays
    n_pois.append(filter_pois_by_radius(osm_poi, location_transformed, radius).shape[0])
   n_schools.append(filter_pois_by_radius(osm_poi_schools,location_transformed,radius).shape[0])
    n_offices.append(filter_pois_by_radius(osm_poi_offices,location_transformed,radius).shape[0])
    n_pub.append(filter_pois_by_radius(osm_poi_pub,location_transformed,radius).shape[0])
    n_college.append(filter_pois_by_radius(osm_poi_college,location_transformed,radius).shape[0])
    n_nursing.append(filter_pois_by_radius(osm_poi_nursing,location_transformed,radius).shape[0])
    n_aerodrome.append(filter_pois_by_radius(osm_poi_aerodrome,location_transformed,radius).shape[0])
    n_bar.append(filter_pois_by_radius(osm_poi_bar,location_transformed,radius).shape[0])
    n_cafe.append(filter_pois_by_radius(osm_poi_cafe,location_transformed,radius).shape[0])
    n_fastfood.append(filter_pois_by_radius(osm_poi_fastfood,location_transformed,radius).shape[0])
    n_restaurant.append(filter_pois_by_radius(osm_poi_restaurant,location_transformed,radius).shape[0])
    n_university.append(filter_pois_by_radius(osm_poi_university,location_transformed,radius).shape[0])
    n_bus_st.append(filter_pois_by_radius(osm_poi_bus_st,location_transformed,radius).shape[0])
    n_parking.append(filter_pois_by_radius(osm_poi_parking,location_transformed,radius).shape[0])
    n_bank.append(filter_pois_by_radius(osm_poi_bank,location_transformed,radius).shape[0])
    n_dentist.append(filter_pois_by_radius(osm_poi_dentist,location_transformed,radius).shape[0])
    n_doctors.append(filter_pois_by_radius(osm_poi_doctors,location_transformed,radius).shape[0])
    n_hospital.append(filter_pois_by_radius(osm_poi_hospital,location_transformed,radius).shape[0])
    n_pharmacy.append(filter_pois_by_radius(osm_poi_pharmacy,location_transformed,radius).shape[0])
    n_veterinary.append(filter_pois_by_radius(osm_poi_veterinary,location_transformed,radius).shape[0])
    n_cinema.append(filter_pois_by_radius(osm_poi_cinema,location_transformed,radius).shape[0])
    n_police.append(filter_pois_by_radius(osm_poi_police,location_transformed,radius).shape[0])
    n_apartment.append(filter_pois_by_radius(osm_poi_apartment,location_transformed,radius).shape[0])
    n_hotel.append(filter_pois_by_radius(osm_poi_hotel,location_transformed,radius).shape[0])
    n_house.append(filter_pois_by_radius(osm_poi_house,location_transformed,radius).shape[0])
    n_residential.append(filter_pois_by_radius(osm_poi_residential,location_transformed,radius).shape[0])
    n_office.append(filter_pois_by_radius(osm_poi_office,location_transformed,radius).shape[0])
    n_motorway.append(filter_pois_by_radius(osm_poi_motorway,location_transformed,radius).shape[0])
    n_trunk.append(filter_pois_by_radius(osm_poi_trunk,location_transformed,radius).shape[0])
    n_primary.append(filter_pois_by_radius(osm_poi_primary,location_transformed,radius).shape[0])
    n_secondary.append(filter_pois_by_radius(osm_poi_secondary,location_transformed,radius).shape[0])
    n_tertiary.append(filter_pois_by_radius(osm_poi_tertiary,location_transformed,radius).shape[0])
    n_pedestrian.append(filter_pois_by_radius(osm_poi_pedestrian,location_transformed,radius).shape[0])
    n_busway.append(filter_pois_by_radius(osm_poi_busway,location_transformed,radius).shape[0])
    n_footway.append(filter_pois_by_radius(osm_poi_footway,location_transformed,radius).shape[0])
    n_path.append(filter_pois_by_radius(osm_poi_path,location_transformed,radius).shape[0])
    n_sidewalk.append(filter_pois_by_radius(osm_poi_sidewalk,location_transformed,radius).shape[0])
    n_cycleway.append(filter_pois_by_radius(osm_poi_cycleway,location_transformed,radius).shape[0])
    n_lane.append(filter_pois_by_radius(osm_poi_lane,location_transformed,radius).shape[0])
    n_bus_stop.append(filter_pois_by_radius(osm_poi_bus_stop,location_transformed,radius).shape[0])
    n_platform.append(filter_pois_by_radius(osm_poi_platform,location_transformed,radius).shape[0])
    n_commercial_land.append(filter_pois_by_radius(osm_poi_commercial_land,location_transformed,radius).shape[0])
    n_edu_land.append(filter_pois_by_radius(osm_poi_edu_land,location_transformed,radius).shape[0])
    n_industrial_land.append(filter_pois_by_radius(osm_poi_industrial_land,location_transformed,radius).shape[0])
    n_residential_land.append(filter_pois_by_radius(osm_poi_residential_land,location_transformed,radius).shape[0])
    n_retail_land.append(filter_pois_by_radius(osm_poi_retail_land,location_transformed,radius).shape[0])
    n_railway_land.append(filter_pois_by_radius(osm_poi_railway_land,location_transformed,radius).shape[0])
    n_fitness.append(filter_pois_by_radius(osm_poi_fitness,location_transformed,radius).shape[0])
    n_sport_centre.append(filter_pois_by_radius(osm_poi_sport_center,location_transformed,radius).shape[0])
    n_swim.append(filter_pois_by_radius(osm_poi_swim,location_transformed,radius).shape[0])
    n_shops.append(filter_pois_by_radius(osm_poi_shops,location_transformed,radius).shape[0])
    n_routes.append(filter_pois_by_radius(osm_poi_routes,location_transformed,radius).shape[0])
    n_sports.append(filter_pois_by_radius(osm_poi_sports,location_transformed,radius).shape[0])

    
    # For saving the poi per location into a geojson file 
    # Format the filename to include latitude and longitude
    # filename = f"Nearby_Schools_{row['latitude']}_{row['longitude']}.geojson"
    # nearby_schools.to_file(filename, driver="GeoJSON")


## Combine the number of POIs for specific tags to the dataset
# Create a column for each tag
location_df["n_pois"] = n_pois
location_df["n_schools"] = n_schools
location_df["n_offices"] = n_offices
location_df["n_pub"] = n_pub
location_df["n_college"] = n_college
location_df["n_nursing"] = n_nursing
location_df["n_aerodrome"] = n_aerodrome
location_df["n_bar"] = n_bar
location_df["n_cafe"] = n_cafe
location_df["n_fastfood"] = n_fastfood
location_df["n_restaurant"] = n_restaurant
location_df["n_university"] = n_university
location_df["n_bus_st"] = n_bus_st
location_df["n_parking"] = n_parking
location_df["n_bank"] = n_bank
location_df["n_dentist"] = n_dentist
location_df["n_doctors"] = n_doctors
location_df["n_hospital"] = n_hospital
location_df["n_pharmacy"] = n_pharmacy
location_df["n_veterinary"] = n_veterinary
location_df["n_cinema"] = n_cinema
location_df["n_police"] = n_police
location_df["n_apartment"] = n_apartment
location_df["n_hotel"] = n_hotel
location_df["n_house"] = n_house
location_df["n_residential"] = n_residential
location_df["n_office"] = n_office
location_df["n_motorway"] = n_motorway
location_df["n_trunk"] = n_trunk
location_df["n_primary"] = n_primary
location_df["n_secondary"] = n_secondary
location_df["n_tertiary"] = n_tertiary
location_df["n_pedestrian"] = n_pedestrian
location_df["n_busway"] = n_busway
location_df["n_footway"] = n_footway
location_df["n_path"] = n_path
location_df["n_sidewalk"] = n_sidewalk
location_df["n_cycleway"] = n_cycleway
location_df["n_lane"] = n_lane
location_df["n_bus_stop"] = n_bus_stop
location_df["n_platform"] = n_platform
location_df["n_commercial_land"] = n_commercial_land
location_df["n_edu_land"] = n_edu_land
location_df["n_industrial_land"] = n_industrial_land
location_df["n_residential_land"] = n_residential_land
location_df["n_retail_land"] = n_retail_land
location_df["n_railway_land"] = n_railway_land
location_df["n_fitness"] = n_fitness
location_df["n_sport_centre"] = n_sport_centre
location_df["n_swim"] = n_swim
location_df["n_shops"] = n_shops
location_df["n_routes"] = n_routes
location_df["n_sports"] = n_sports

# Rename a new file
location_df.to_csv("stop_25_dr",sep=";")

# See the data
location_df.head()
