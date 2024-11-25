import random
import grpc
from concurrent import futures
import humidity_pb2, humidity_pb2_grpc

class HumidityServiceServicer(humidity_pb2_grpc.HumidityServiceServicer):
    def GetHumidity(self, request, context):
        humidity = round(random.uniform(30, 80), 2) 
        return humidity_pb2.HumidityResponse(humidity=humidity)

def serve():
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    humidity_pb2_grpc.add_HumidityServiceServicer_to_server(HumidityServiceServicer(), server)
    server.add_insecure_port('[::]:50052')
    server.start()
    server.wait_for_termination()

if __name__ == "__main__":
    serve()