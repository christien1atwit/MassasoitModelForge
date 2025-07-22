import requests
import pandas as pd
from datetime import datetime
import numpy as np

class NOAANCEIClient:
    def __init__(self):
        self.base_url = "https://www.ncei.noaa.gov/cdo-web/api/v2/"
        self.token = "YOUR_TOKEN_HERE"  # User will need to provide this
        self.headers = {
            "token": self.token
        }

    def get_stations(self, location=None, dataset_id="GHCND"):
        """
        Get available stations for a given location
        """
        params = {
            "datasetid": dataset_id,
            "limit": 1000
        }
        if location:
            params["locationid"] = location
        
        response = requests.get(
            f"{self.base_url}stations",
            headers=self.headers,
            params=params
        )
        response.raise_for_status()
        return response.json()

    def get_weather_data(self, station_id, start_date, end_date):
        """
        Get daily weather data for a specific station
        """
        params = {
            "stationid": station_id,
            "startdate": start_date,
            "enddate": end_date,
            "limit": 1000,
            "units": "standard"
        }
        
        response = requests.get(
            f"{self.base_url}data",
            headers=self.headers,
            params=params
        )
        response.raise_for_status()
        return response.json()

    def process_weather_data(self, weather_data):
        """
        Process raw weather data into a pandas DataFrame
        """
        records = []
        for record in weather_data["results"]:
            date = datetime.strptime(record["date"], "%Y-%m-%dT%H:%M:%S")
            records.append({
                "date": date.date(),
                "tmax": record.get("TMAX", np.nan),
                "tmin": record.get("TMIN", np.nan),
                "prcp": record.get("PRCP", np.nan),
                "snow": record.get("SNOW", np.nan),
                "snwd": record.get("SNWD", np.nan)
            })
        
        return pd.DataFrame(records)

    def get_location_by_coordinates(self, latitude, longitude):
        """
        Get location ID by coordinates
        """
        # Convert coordinates to string format
        lat_str = f"{latitude:.4f}"
        lon_str = f"{longitude:.4f}"
        
        params = {
            "locationcategoryid": "CITY",
            "radius": f"{lat_str},{lon_str},50",
            "limit": 1
        }
        
        response = requests.get(
            f"{self.base_url}locations",
            headers=self.headers,
            params=params
        )
        response.raise_for_status()
        
        # Extract the location ID from the response
        if response.json().get('results'):
            return response.json()['results'][0]['id']
        return None
