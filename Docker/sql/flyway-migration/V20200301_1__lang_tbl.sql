CREATE TABLE language_tbl (
    id bigint DEFAULT nextval('pk_sequence')	 NOT NULL,
    name text NOT NULL,
    CONSTRAINT language_pk PRIMARY KEY (id),
    CONSTRAINT language_name_unq UNIQUE (name)
);


-- CREATE INDEX wiki_language_name_idx ON language_tbl(name);