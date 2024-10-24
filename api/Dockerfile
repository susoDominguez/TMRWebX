# Use a specific version of the node image with an updated SHA for security purposes
FROM node:lts-alpine@sha256:76badf0d0284ad838536d49d8a804988b4985fc6bc7242dfff4f8216c851438b

# Install dumb-init to handle PID 1 signals gracefully
RUN apk add --no-cache dumb-init

# Set environment variables for production mode
ARG NODE_ENV=development
ENV NODE_ENV=${NODE_ENV}

# Set working directory
WORKDIR /usr/src/app

# Copy package.json and package-lock.json and install dependencies
COPY --chown=node:node package*.json ./

# Install dev dependencies in development and production dependencies in production
RUN if [ "$NODE_ENV" = "production" ]; then npm ci --only=production; else npm install; fi

# Copy the rest of the app’s source code
COPY --chown=node:node . .

# Switch to a non-root user for security reasons
USER node

# Expose the port the app runs on
EXPOSE 8888

# Use dumb-init as the entrypoint for proper signal handling and to avoid zombie processes
ENTRYPOINT ["dumb-init", "--"]

# Run the app
CMD ["node", "./bin/www"]
