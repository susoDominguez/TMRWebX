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
ENTRYPOINT ["swipl",   "-l", "server.pl"]
CMD [ "-g", "server(1234)" ]
#CMD ["swipl", "-g", "server(1234)", "--port=1234", "--stand_alone=true", "-t", "server.pl"]
#ENTRYPOINT ["swipl", "-f", "–user=daemon", "–-fork=true", "-port=1234", "server.pl", "-g", "server(1234)"]