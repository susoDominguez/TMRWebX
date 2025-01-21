# Use the latest stable SWI-Prolog image
FROM swipl:latest

# Set the working directory
WORKDIR /usr/server/backend/

# Set build-time arguments with defaults
ARG buildtime_FUSEKI_PORT=3030
ARG buildtime_FUSEKI_HOST=store_service

# Configure environment variables
ENV HOST=${buildtime_FUSEKI_HOST}
ENV PORT=${buildtime_FUSEKI_PORT}
ENV FUSEKI_HOST_PORT=http://${HOST}:${PORT}/

# Copy only necessary files to reduce image size
COPY . .

# Install necessary dependencies for compatibility (including ARM64)
RUN apt-get update && apt-get install -y \
    build-essential \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Expose the Prolog server port
EXPOSE 1234

# Default command to start the Prolog server
ENTRYPOINT ["swipl", "-l", "server.pl"]
CMD ["-g", "server(1234)"]

#CMD ["swipl", "-g", "server(1234)", "--port=1234", "--stand_alone=true", "-t", "server.pl"]
#ENTRYPOINT ["swipl", "-f", "–user=daemon", "–-fork=true", "-port=1234", "server.pl", "-g", "server(1234)"]

# build for arm64:
# docker buildx build --platform linux/arm64 -t reasoner_img --load .