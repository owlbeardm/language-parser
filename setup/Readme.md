## Database update

Run flyway migration (create instance locally and updates RDS Database instance):

```
docker run -it --rm -v "$(pwd)/sql/flyway-migration:/flyway/sql" --network="host" flyway/flyway:6.0.1 -url=jdbc:postgresql://172.20.7.103:5433/wiki -user=wiki -password=wiki migrate
```