FROM python:3.9

WORKDIR /app

COPY . .

RUN pip install grpcio grpcio-tools

EXPOSE 50052

CMD ["python", "humidity.py"]