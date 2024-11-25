import random
import grpc
from concurrent import futures
import temperature_pb2, temperature_pb2_grpc

class TemperatureServiceServicer(temperature_pb2_grpc.TemperatureServiceServicer):
    def GetTemperature(self, request, context):
        temperature = round(random.uniform(18, 30), 2)
        return temperature_pb2.TemperatureResponse(temperature=temperature)

def serve():
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    temperature_pb2_grpc.add_TemperatureServiceServicer_to_server(TemperatureServiceServicer(), server)
    server.add_insecure_port('[::]:50051')
    server.start()
    server.wait_for_termination()

if __name__ == "__main__":
    serve()