# Use a specific version of the Node.js image (lightweight)
FROM node:23-alpine

# Install dumb-init for proper signal handling
RUN apk add --no-cache dumb-init

# Define build arguments and set environment variables
ARG NODE_ENV=production
ARG SKIP_COPY=false 
ENV NODE_ENV=${NODE_ENV}

# Set working directory
WORKDIR /usr/src/app

# Copy package.json and package-lock.json for dependency installation (cached layer)
COPY --chown=node:node package*.json ./

# Install dependencies based on the environment
RUN --mount=type=cache,target=/root/.npm \
    if [ "$NODE_ENV" = "production" ]; then \
    npm ci --only=production; \
    else \
    npm install; \
    fi

# Copy the rest of the application code (conditionally skipped in development)
RUN test "$SKIP_COPY" = "true" || echo "Copying source code..."
COPY --chown=node:node . ./

# Ensure proper file ownership for security
RUN chown -R node:node /usr/src/app

# Switch to a non-root user for security
USER node

# Expose the application port
EXPOSE 8888

# Use dumb-init as the entrypoint for proper signal handling
ENTRYPOINT ["dumb-init", "--"]

# Default command for production or development
CMD ["sh", "-c", "if [ \"$NODE_ENV\" = \"development\" ]; then npm run dev; else npm start; fi"]