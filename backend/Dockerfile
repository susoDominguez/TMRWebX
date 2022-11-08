FROM swipl:latest
#RUN apt update -y && \
#    apt install -y git
WORKDIR /usr/server/backend/

#ARG buildtime_FUSEKI_PORT=3030
#ARG buildtime_FUSEKI_HOST=fuseki

#ENV HOST=${FUSEKI_HOST:-${buildtime_FUSEKI_HOST}}
#ENV PORT=${FUSEKI_PORT:-${buildtime_FUSEKI_PORT}}
#ENV FUSEKI_HOST_PORT=http://${HOST}:${PORT}/
COPY . .
#clone tmr repo with ontologies

EXPOSE 1234
#RUN "cd /app"
# Keep container alive, retaining background server process, with sleep (temporarily).
ENTRYPOINT ["swipl",   "-l", "server.pl"]
CMD [ "-g", "server(1234)" ]
#CMD ["swipl", "-g", "server(1234)", "--port=1234", "--stand_alone=true", "-t", "server.pl"]
#ENTRYPOINT ["swipl", "-f", "–user=daemon", "–-fork=true", "-port=1234", "server.pl", "-g", "server(1234)"]