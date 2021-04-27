FROM stain/jena-fuseki
EXPOSE 3030
# Adds curl
RUN apk add --update curl && rm -rf /var/cache/apk/*

FROM busybox
COPY ./my-static-binary /my-static-binary
CMD ["/my-static-binary"]

COPY --chown=9008 ttl/people /tmp/people
COPY --chown=9008 ttl/places /tmp/places

RUN cat /tmp/people/*.ttl > /tmp/people.ttl
RUN cat /tmp/places/*.ttl > /tmp/places.ttl

RUN $TDBLOADER --graph=http://ldf.fi/congress/people /tmp/people.ttl \
	&& $TDBLOADER --graph=http://ldf.fi/congress/places /tmp/places.ttl \
	&& $TEXTINDEXER \
	&& $TDBSTATS --graph urn:x-arq:UnionGraph > /tmp/stats.opt \
	&& mv /tmp/stats.opt /fuseki-base/databases/tdb/ \
	&& rm -r /tmp/*