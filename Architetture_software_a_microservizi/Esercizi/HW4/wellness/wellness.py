from flask import Flask, jsonify
import grpc
import temperature_pb2, temperature_pb2_grpc, humidity_pb2, humidity_pb2_grpc

app = Flask(__name__)

def get_temperature():
    with grpc.insecure_channel('temperature:50051') as channel:
        stub = temperature_pb2_grpc.TemperatureServiceStub(channel)
        response = stub.GetTemperature(temperature_pb2.EmptyT())
        return response.temperature

def get_humidity():
    with grpc.insecure_channel('humidity:50052') as channel:
        stub = humidity_pb2_grpc.HumidityServiceStub(channel)
        response = stub.GetHumidity(humidity_pb2.EmptyH())
        return response.humidity

@app.route('/wellness', methods=['GET'])
def read_wellness():
    temperature = get_temperature()
    humidity = get_humidity()
    
    # Basic wellness calculation
    wellness = 100 - abs(temperature - 22) - abs(humidity - 50) / 2
    wellness_score = max(min(wellness, 100), 0)
    
    return jsonify({
        "temperature": temperature,
        "humidity": humidity,
        "wellness": round(wellness_score, 2)
    })

if __name__ == "__main__":
    app.run(host='0.0.0.0', port=8000, debug=True)