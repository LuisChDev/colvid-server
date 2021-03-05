FROM colvid-server-docker:latest
COPY posters /var/assets
COPY testdb.sqlite /var/db
