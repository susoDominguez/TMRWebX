# Use the latest stable SWI-Prolog image
FROM swipl:latest

# Set the working directory
WORKDIR /usr/server/backend/

# Set build-time arguments with defaults
ARG buildtime_FUSEKI_PORT=3030
ARG buildtime_FUSEKI_HOST=store_service

# Configure environment variables
ENV FUSEKI_HOST=${buildtime_FUSEKI_HOST}
ENV FUSEKI_PORT=${buildtime_FUSEKI_PORT}
ENV FUSEKI_HOST_PORT=http://${FUSEKI_HOST}:${FUSEKI_PORT}/

# Copy only necessary files to reduce image size
COPY server.pl .
COPY util.pl .
COPY setup.pl .
COPY current_logic/ ./current_logic/
#COPY dependencies/ ./dependencies/  

# Install necessary dependencies (ensures compatibility with ARM64)
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Ensure correct permissions
RUN chmod -R 755 /usr/server/backend/

# Expose the Prolog server port
EXPOSE 1234

# Default command to start the Prolog server
ENTRYPOINT ["swipl", "-l", "server.pl"]
CMD ["-g", "server(1234)"]

# Notes:
# - Build for ARM64:
#   docker buildx build --platform linux/arm64 -t reasoner_img --load .