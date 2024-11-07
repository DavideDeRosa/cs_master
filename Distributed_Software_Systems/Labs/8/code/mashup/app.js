// Select the HTML elements
const currentWeatherData = document.getElementById("current-weather-data");
const hourlyForecastData = document.getElementById("hourly-forecast-data");
const fiveDayForecastData = document.getElementById("five-day-forecast-data");

// API endpoints
const API_URL_CURRENT = "https://api.openweathermap.org/data/2.5/weather?lat=44.4833&lon=11.3333&appid=f7ec15dcba5d962240ea32916bf538ae&units=metric";
const API_URL_FORECAST = "https://api.weatherapi.com/v1/forecast.json?q=Bologna&days=2&key=c804c071bc1447589eb170658240511";
const API_URL_5DAYS = "https://api.weatherapi.com/v1/forecast.json?q=Bologna&days=6&key=c804c071bc1447589eb170658240511";

// Current Weather Data
async function fetchCurrentWeather() {
    try {
        const response = await fetch(API_URL_CURRENT);
        const data = await response.json();
        displayCurrentWeather(data);
    } catch (error) {
        console.error("Error fetching current weather:", error);
    }
}

// Display Current Weather Data
function displayCurrentWeather(data) {
    const temperature = data.main.temp;
    const feelsLike = data.main.feels_like;
    const weatherCondition = data.weather[0].main;
    const description = data.weather[0].description;
    const iconUrl = `http://openweathermap.org/img/wn/${data.weather[0].icon}@2x.png`;
    const humidity = data.main.humidity;
    const windSpeed = data.wind.speed;
    const windDirection = data.wind.deg;
    const visibility = data.visibility / 1000;
    const rainLastHour = data.rain ? data.rain["1h"] : 0;

    // Render HTML content
    currentWeatherData.innerHTML = `
        <img src="${iconUrl}" alt="${description}" />
        <p><strong>Temperature:</strong> ${temperature}°C</p>
        <p><strong>Feels Like:</strong> ${feelsLike}°C</p>
        <p><strong>Condition:</strong> ${weatherCondition} (${description})</p>
        <p><strong>Humidity:</strong> ${humidity}%</p>
        <p><strong>Wind:</strong> ${windSpeed} m/s, ${windDirection}°</p>
        <p><strong>Visibility:</strong> ${visibility} km</p>
        <p><strong>Rain (last hour):</strong> ${rainLastHour} mm</p>
    `;
}

// Hourly Forecast Data
async function fetchHourlyForecast() {
    try {
        const response = await fetch(API_URL_FORECAST);
        const data = await response.json();
        displayHourlyForecast(data);
    } catch (error) {
        console.error("Error fetching hourly forecast:", error);
    }
}

// Display Hourly Forecast Data
function displayHourlyForecast(data) {
    const hoursDay1 = data.forecast.forecastday[0].hour;
    const hoursDay2 = data.forecast.forecastday[1].hour;
    const allHours = [...hoursDay1, ...hoursDay2];

    const currentTimeEpoch = data.location.localtime_epoch;

    const startIndex = allHours.findIndex(hour => hour.time_epoch >= currentTimeEpoch);

    const forecastHTML = allHours.slice(startIndex, startIndex + 24).map(hour => {
        const time = new Date(hour.time).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
        const temp = hour.temp_c.toFixed(1);
        const condition = hour.condition.text;
        const iconUrl = `https:${hour.condition.icon}`;
        const windSpeed = hour.wind_kph;
        const rain = hour.chance_of_rain

        return `
            <div class="forecast-item">
                <p><strong>${time}</strong></p>
                <img src="${iconUrl}" alt="${condition}" />
                <p><strong>Temp:</strong> ${temp}°C</p>
                <p><strong>Cond:</strong> ${condition}</p>
                <p><strong>Wind:</strong> ${windSpeed} kph</p>
                <p><strong>Rain:</strong> ${rain}%</p>
            </div>
        `;
    }).join("");

    // Render HTML content
    hourlyForecastData.innerHTML = forecastHTML;
}

// 5-Day Forecast Data
async function fetchFiveDayForecast() {
    try {
        const response = await fetch(API_URL_5DAYS);
        const data = await response.json();
        displayFiveDayForecast(data);
    } catch (error) {
        console.error("Error fetching 5-day forecast:", error);
    }
}

// Display 5-Day Forecast Data
function displayFiveDayForecast(data) {
    const lastFiveDays = data.forecast.forecastday.slice(1, 6);
    
    console.log(lastFiveDays)

    const forecastHTML = lastFiveDays.map(day => `
        <div class="forecast-item">
            <p><strong>${day.date}</strong></p>
            <img src="https:${day.day.condition.icon}" alt="Weather Icon"/>
            <p><strong>Max Temp:</strong> ${day.day.maxtemp_c}°C</p>
            <p><strong>Min Temp:</strong> ${day.day.mintemp_c}°C</p>
            <p><strong>Cond:</strong> ${day.day.condition.text}</p>
            <p><strong>Rain:</strong> ${day.day.daily_chance_of_rain}%</p>
        </div>
    `).join("");

    // Render HTML content
    fiveDayForecastData.innerHTML = forecastHTML;
}

// Initialize Fetch Functions
fetchCurrentWeather();
fetchHourlyForecast();
fetchFiveDayForecast();