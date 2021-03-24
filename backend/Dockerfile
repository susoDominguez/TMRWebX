FROM swipl
WORKDIR /usr/server/backend/
#dev
#ENV FUSEKI_HOST_PORT=http://localhost:3030/
#production
ENV FUSEKI_HOST_PORT=http://fuseki:3030/
COPY . .
EXPOSE 1234
#RUN "cd /app"
# Keep container alive, retaining background server process, with sleep (temporarily).
#CMD ["swipl", "-f", "app/server.pl", "-g", "server(1234)", "-g", "sleep(500000)"]
ENTRYPOINT ["swipl"] 
#CMD ["server.pl", "–user=daemon", "–fork=false", "–port=1234"]
CMD ["server.pl", "–user=daemon", "–fork=false", "-g", "server(1234)", "-g", "sleep(500000)"]