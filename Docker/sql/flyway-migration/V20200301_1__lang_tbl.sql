CREATE TABLE language_tbl (
    id bigint NOT NULL,
    version bigint DEFAULT 1 NOT NULL,
    createdby text NOT NULL,
    createdwhen timestamp without time zone NOT NULL,
    modiby text NOT NULL,
    modiwhen timestamp without time zone NOT NULL,
    name text NOT NULL,
    CONSTRAINT language_pk PRIMARY KEY (id),
    CONSTRAINT language_name_unq UNIQUE (name)
);


-- CREATE INDEX wiki_language_name_idx ON language_tbl(name);