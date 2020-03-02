# Make dump 
```
pg_dump --cluster 11/main -h 172.20.7.103 -p 5432 -d wiki -U wiki  > dumpfile.sql
```
`--clean` option for emptying db

# restore

```
psql --cluster 11/main -h 172.20.7.103 -p 5432 -d wiki -U wiki < dumpfile.sql
```