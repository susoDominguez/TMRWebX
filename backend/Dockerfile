# Pin to a specific SWI-Prolog version for stability
FROM swipl:8.2.4

# Set the working directory
WORKDIR /usr/server/backend/

# Set build-time and runtime environment variables
ARG buildtime_FUSEKI_PORT=3012
ARG buildtime_FUSEKI_HOST=fuseki

# Set environment variables for runtime
ENV HOST=${buildtime_FUSEKI_HOST}
ENV PORT=${buildtime_FUSEKI_PORT}
ENV FUSEKI_HOST_PORT=http://${HOST}:${PORT}/

# Copy the entire backend project
COPY . .

# Expose the Prolog server port
EXPOSE 1234

# Start the Prolog server when the container starts
ENTRYPOINT ["swipl", "-l", "server.pl"]
CMD [ "-g", "server(1234)" ]

#CMD ["swipl", "-g", "server(1234)", "--port=1234", "--stand_alone=true", "-t", "server.pl"]
#ENTRYPOINT ["swipl", "-f", "–user=daemon", "–-fork=true", "-port=1234", "server.pl", "-g", "server(1234)"]