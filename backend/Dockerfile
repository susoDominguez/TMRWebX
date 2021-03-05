FROM swipl
COPY . /app
ENV FUSEKI_HOST_PORT=http://fuseki:3030/
RUN "cd /app"
# Keep container alive, retaining background server process, with sleep (temporarily).
CMD ["swipl", "-f", "./server.pl", "-g", "server(1234)", "-g", "sleep(500000)"]
