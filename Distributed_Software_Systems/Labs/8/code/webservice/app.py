from flask import Flask, request, jsonify
import requests
from config import API_KEY, API_URL

app = Flask(__name__)

"""
Function that executes the requests to the WeatherAPI and return the data
"""
def fetch_weather_data(endpoint, params):
    params['key'] = API_KEY
    response = requests.get(f"{API_URL}/{endpoint}.json", params=params)

    return response.json() if response.status_code == 200 else None

"""
Endpoint for the Current Weather. The user gives the city name as a query parameter and gets the current weather data
"""
@app.route('/weather/current', methods=['GET'])
def current_weather():
    city = request.args.get('city')

    if not city:
        return jsonify({"error": "Please specify a city."}), 400
    
    params = {'q': city}
    data = fetch_weather_data(endpoint='current', params=params)

    if data:
        response = {
            "location": data["location"]["name"] + ", " + data["location"]["region"]+ ", " + data["location"]["country"],
            "temperature": data["current"]["temp_c"],
            "description": data["current"]["condition"]["text"],
            "humidity": data["current"]["humidity"],
            "wind_speed": data["current"]["wind_kph"],
            "rain_mm": data["current"]["precip_mm"]
        }

        return jsonify(response)
    else:
        return jsonify({"error": "City not found or service unavailable."}), 404

"""
Endpoint for the Hourly Weather. The user gives the city name as a query parameter and gets the hourly weather data
"""
@app.route('/weather/hourly', methods=['GET'])
def hourly_forecast():
    city = request.args.get('city')

    if not city:
        return jsonify({"error": "Please specify a city."}), 400
    
    params = {'q': city}
    data = fetch_weather_data('forecast', params)

    if data:
        hourly_data = [
            {
                "time": data["forecast"]["forecastday"][0]["hour"][i]["time"],
                "temperature": data["forecast"]["forecastday"][0]["hour"][i]["temp_c"],
                "description": data["forecast"]["forecastday"][0]["hour"][i]["condition"]["text"],
                "humidity": data["forecast"]["forecastday"][0]["hour"][i]["humidity"],
                "wind_speed": data["forecast"]["forecastday"][0]["hour"][i]["wind_kph"],
                "rain_mm": data["forecast"]["forecastday"][0]["hour"][i]["precip_mm"]
            }
            for i in range (24)
        ]

        response = {"location": data["location"]["name"] + ", " + data["location"]["region"]+ ", " + data["location"]["country"], 
                    "date": data["forecast"]["forecastday"][0]["date"],
                    "hourly_forecast": hourly_data}
        
        return jsonify(response)
    else:
        return jsonify({"error": "City not found or service unavailable."}), 404

"""
Endpoint for the Daily Weather. The user gives the city name and the number of days as a query parameter and gets the daily weather data for the number of days specified
"""
@app.route('/weather/daily', methods=['GET'])
def daily_forecast():
    city = request.args.get('city')
    days = int(request.args.get('days'))
    
    if not city:
        return jsonify({"error": "Please specify a city."}), 400
    
    params = {'q': city, 'days': days}
    data = fetch_weather_data('forecast', params)

    if data:
        daily_data = [
            {
                "date": data["forecast"]["forecastday"][day]["date"],
                "max_temp": data["forecast"]["forecastday"][day]["day"]["maxtemp_c"],
                "min_temp": data["forecast"]["forecastday"][day]["day"]["mintemp_c"],
                "condition": data["forecast"]["forecastday"][day]["day"]["condition"]["text"],
                "rain": data["forecast"]["forecastday"][day]["day"]["daily_chance_of_rain"]
            }
            for day in range(days)
        ]

        response = {"location": data["location"]["name"] + ", " + data["location"]["region"]+ ", " + data["location"]["country"], 
                    "daily_forecast": daily_data}
        
        return jsonify(response)
    else:
        return jsonify({"error": "City not found or service unavailable."}), 404

if __name__ == "__main__":
    app.run(port=8000, debug=True)