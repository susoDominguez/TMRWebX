FROM node:lts-alpine@sha256:76badf0d0284ad838536d49d8a804988b4985fc6bc7242dfff4f8216c851438b
RUN apk add dumb-init
ENV NODE_ENV=production
# set a directory for the app
WORKDIR /usr/src/app

COPY --chown=node:node package*.json ./
RUN npm install
#RUN npm ci --only=production
#install rest of project
COPY --chown=node:node . .
# install dependencies
USER node
# define the port number the container should expose
EXPOSE 8888
# run the command
CMD ["dumb-init", "node", "./bin/www"]
