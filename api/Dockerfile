# Use a specific version of the Node.js image
FROM node:lts-alpine@sha256:76badf0d0284ad838536d49d8a804988b4985fc6bc7242dfff4f8216c851438b

# Install dumb-init for proper signal handling
RUN apk add --no-cache dumb-init

# Define build arguments and set environment variables
ARG NODE_ENV=production
ENV NODE_ENV=${NODE_ENV}

# Set working directory
WORKDIR /usr/src/app

# Copy package.json and package-lock.json for dependency installation
COPY --chown=node:node package*.json ./

# Install dependencies based on the environment
RUN if [ "$NODE_ENV" = "production" ]; then \
    npm ci --only=production && npm cache clean --force; \
    else \
    npm install && npm cache clean --force; \
    fi

# Copy the rest of the application code (conditionally skipped in development)
ARG SKIP_COPY=false
COPY --chown=node:node . ./

# Switch to a non-root user for security
USER node

# Expose the application port
EXPOSE 8888

# Use dumb-init as the entrypoint for proper signal handling
ENTRYPOINT ["dumb-init", "--"]

# Default command for production or development
CMD ["sh", "-c", "if [ \"$NODE_ENV\" = \"development\" ]; then npm run devstart; else npm start; fi"]